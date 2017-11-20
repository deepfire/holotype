{-# LANGUAGE DataKinds, GADTs, KindSignatures, TypeFamilies #-}
{-# LANGUAGE DeriveFunctor, GeneralizedNewtypeDeriving, StandaloneDeriving #-}
{-# LANGUAGE FlexibleContexts, FlexibleInstances #-}
{-# LANGUAGE LambdaCase, MultiWayIf, OverloadedStrings, RecordWildCards, ScopedTypeVariables, TupleSections #-}
{-# LANGUAGE UnicodeSyntax #-}
{-# OPTIONS_GHC -Wall -Wno-unticked-promoted-constructors -Wno-orphans #-}

module HoloFont
  ( Font(..), FKind(..)
  , FontSizeRequest(..)
  , FamilyName, FaceName
  , LayoutHeightLimit(..)
  , TextSizeSpec(..), tssWidth, tssHeight

  -- * Query
  , fmDefault
  , fmFamilies, fmResolution
  , ffamName, ffamFaces
  , ffaceName, ffacePISizes
  , fontQuerySize

  -- * Main API
  , validateFont, chooseFont, bindFont

  -- * Ancillary
  , fdSetSize

  -- * Text
  , makeTextLayout, laySetWidth, laySetHeight, laySetSize, layGetSize, laySetHeightLimit
  , layPrintLimits , layRunTextForSize
  , layDrawText

  -- * FontMap
  , FontKey(..), FontAlias(..), FontPreferences(..), FontMap(..)
  , makeFontMap, lookupFont, lookupFont'
  )
where

-- Basis
import           HoloPrelude                       hiding ((.))
import           Prelude                           hiding (fail)
import           Control.Arrow                            ((***))

-- Types
import           Control.Monad                            (foldM)
import           Control.Monad.IO.Class                   (MonadIO, liftIO)
import           Data.Either
import           Data.Either.Extra
import           Data.Map                                 (Map)
import           Data.Ord
import qualified Data.Map                          as Map
import           Data.Maybe                               (fromMaybe)
import           Data.Text                         as T   (Text, unpack)

-- Algebra
import           Linear

-- Misc
import           Text.Printf                              (printf)

-- Manually-bound Cairo
import qualified Graphics.Rendering.Cairo          as GRC (moveTo)

-- glib-introspection -based Cairo and Pango
import qualified GI.Cairo                          as GIC
import qualified GI.Pango                          as GIP
import qualified GI.PangoCairo.Interfaces.FontMap  as GIPC
import qualified GI.PangoCairo.Functions           as GIPC

-- Dirty stuff
import qualified System.IO.Unsafe                  as UN

-- Local imports
import Flatland
import FlatDraw
import HoloCairo


-- $Font choice strategy.
--
-- The current font choice strategy is based on preference lists -- a list of
-- font specification tuples of (family, face, size, size-policy), where:
--   - 'size' is either in Pango units or in typographic points (at 72ppi)
--   - 'size-policy', when specified (as one of LT, EQ, GT), indicates a preference
--     for bitmapped fonts, and thereso specifies a decision procedure, for when an
--     exact size match is unavailable -- nearest smaller, failure, nearest larger.
--
-- Such a preference list is crossed against a fontmap, and the first matching
-- font is chosen.
--
-- Note, that this strategy has no explicit means to facilitate reliable character
-- display -- since, probably no known font provides for all Unicode characters.
-- The fontconfig mechanism underlying pangocairo font /might/ be able to help,
-- although it will likely need more work to engage that functionality.
--
-- If/when we decide to embark upon that journey, 'Pango.Objects.Context.contextLoadFontset'
-- would be our first point of contact.
--
-- $Known deficiencies.
--
-- 1. This library is specific to GI.PangoCairo.Context, which is a specialization of GI.Pango.Context.
--

-- Note [Fontmap type]
-- ~~~~~~~~~~~~~~~~~~~
-- We choose to entirely avoid GI.Pango.FontMap,
-- because it has no intrinsic concept of resolution, which we heavily rely upon.
-- Hence, we opt to GI.PangoCairo.FontMap everywhere.
--
instance Show GIPC.FontMap where
  show = printf "FontMap { resolution = %fDΠ }" ∘ fromDΠ ∘ fmResolution
instance Show GIP.FontFamily where
  show = printf "FontFamily { name = '%s' }" ∘ ffamName
instance Show GIP.FontFace where
  show = printf "FontFace { name = '%s' }" ∘ ffaceName

fmDefault ∷ GIPC.FontMap
fmDefault = GIPC.fontMapGetDefault & UN.unsafePerformIO

fmFamilies ∷ GIP.IsFontMap a ⇒ a → [GIP.FontFamily]
fmFamilies = UN.unsafePerformIO ∘ GIP.fontMapListFamilies

-- | Get fontmap resolution.
-- This function isn't in IO, because the resolution mutation function isn't defined.
-- If ever something is exported that changes the fontmap resolution, this function's
-- signature must be moved to IO.
fmResolution ∷ GIPC.FontMap → DΠ
fmResolution = DΠ ∘ UN.unsafePerformIO ∘ GIPC.fontMapGetResolution
-- fmSetResolution ∷ GIPC.FontMap → DΠ → IO ()
-- fmSetResolution fm = GIPC.fontMapSetResolution fm ∘ fromDΠ

-- pcResolution ∷ GIP.Context → IO DΠ
-- pcResolution ctx = DΠ <$> GIPC.contextGetResolution ctx
-- pcSetResolution ∷ GIP.Context → DΠ → IO ()
-- pcSetResolution ctx = GIPC.contextSetResolution ctx ∘ fromDΠ

ffamName ∷ GIP.FontFamily → Text
ffamName = UN.unsafePerformIO ∘ GIP.fontFamilyGetName
ffamFaces ∷ GIP.FontFamily → [GIP.FontFace]
ffamFaces = UN.unsafePerformIO ∘ GIP.fontFamilyListFaces

ffaceName ∷ GIP.FontFace → Text
ffaceName = UN.unsafePerformIO ∘ GIP.fontFaceGetFaceName
ffacePISizes ∷ GIP.FontFace → Maybe [Unit PUI]
ffacePISizes fface = (fromIntegral <$>) <$> (UN.unsafePerformIO (GIP.fontFaceListSizes fface))


-- * Font sizes

-- | Set font size: XXX/upstream/inconsistency -- Double, yet PANGO_SCALE-d:
--   https://hackage.haskell.org/package/gi-pango-1.0.11/docs/GI-Pango-Structs-FontDescription.html#v:fontDescriptionSetAbsoluteSize
--   https://hackage.haskell.org/package/gi-pango-1.0.11/docs/GI-Pango-Structs-FontDescription.html#v:fontDescriptionSetSize
fdSetSize ∷ (MonadIO m) ⇒ GIP.FontDescription → Unit u → m ()
fdSetSize fd (PUs  us)  = GIP.fontDescriptionSetAbsoluteSize fd $ us * fromIntegral GIP.SCALE
fdSetSize fd (PUIs is)  = GIP.fontDescriptionSetAbsoluteSize fd $ fromIntegral is
fdSetSize fd (Pts  pts) = GIP.fontDescriptionSetSize         fd $ pts * GIP.SCALE

data FontSizeRequest u where
  FSROutline ∷ FromUnit u ⇒
    { fsValue   ∷ Unit u
    } → FontSizeRequest u
  FSRBitmap  ∷ FromUnit u ⇒
    { fsValue   ∷ Unit u
    , fsbPolicy ∷ Ordering
    } → FontSizeRequest u
deriving instance Show (FontSizeRequest u)

--- XXX/expressivity:
-- let ascending = True in sortOn (if ascending then id else Down)
--    [0, 2, 1]
--     • Occurs check: cannot construct the infinite type: a ~ Down a
--       Expected type: Down a -> Down a
--         Actual type: a -> Down a
--     • In the expression: Down
--       In the first argument of ‘sortOn’, namely
--         ‘(if o then id else Down)’
--       In the expression: sortOn (if o then id else Down) [0, 2, 1]


newtype FamilyName = FamilyName { fromFamilyName ∷ Text } deriving (Eq, Show, IsString) -- ^ Pango font family name
newtype FaceName   = FaceName   { fromFaceName   ∷ Text } deriving (Eq, Show, IsString) -- ^ Pango font face name

data FKind = Spec | Found | Bound

data Font (k ∷ FKind) u where
  FontSpec ∷
    { frFamilyName     ∷ FamilyName
    , frFaceName       ∷ FaceName
    , frSizeRequest    ∷ FontSizeRequest u
    } → Font Spec u
  Font ∷
    { fFamilyName      ∷ FamilyName
    , fFaceName        ∷ FaceName
    , fSize            ∷ Unit u
    , fDΠ              ∷ DΠ
    , fMaxHeight       ∷ He (Unit u)
    --
    , fDesc            ∷ GIP.FontDescription
    , fFontMap         ∷ GIPC.FontMap
    , fDetachedContext ∷ GIP.Context
    , fDetachedLayout  ∷ GIP.Layout
    } → Font Found u
  FontBinding ∷
    { fbFont           ∷ Font Found u
    , fbContext        ∷ GIP.Context
    } → Font Bound u
deriving instance Eq   (FontSizeRequest u)

instance Show (Font Spec u) where
  show FontSpec{..} =
    printf "FontSpec { family = %s, face = %s, size = %s }"
    (fromFamilyName frFamilyName) (fromFaceName frFaceName) (show frSizeRequest)
instance Show (Font Found u) where
  show Font{..} =
    printf "Font { family = %s, face = %s, size = %s }"
    (fromFamilyName fFamilyName) (fromFaceName fFaceName) (show fSize)

instance Eq   (Font Spec  u) where
  FontSpec famnl facnl fsrl          == FontSpec famnr facnr fsrr =
    famnl ≡ famnr ∧ facnl ≡ facnr ∧ fsrl ≡ fsrr
instance Eq   (Font Found u) where
  Font famnl facnl fszl fdπl _ _ _ _ _ == Font famnr facnr fszr fdπr _ _ _ _ _ =
    famnl ≡ famnr ∧ facnl ≡ facnr ∧ fszl ≡ fszr ∧ fdπl ≡ fdπr
instance Eq   (Font Bound u) where
  FontBinding fl _                   == FontBinding fr _ =
    fl ≡ fr

validateFont ∷ (MonadIO m) ⇒ FromUnit u ⇒ GIPC.FontMap → Font Spec u → m (Either String (Font Found u))
validateFont fFontMap (FontSpec
                       fFamilyName@(FamilyName ffamname)
                       fFaceName@(FaceName ffacename)
                       fSizeRequest) =
  let fams  = filter ((≡ ffamname)  ∘ ffamName) $ fmFamilies fFontMap
      faces = filter ((≡ ffacename) ∘ ffaceName) $ concat $ ffamFaces <$> fams
      fDΠ   = fmResolution fFontMap
      fPI   = fromUnit fDΠ $ fsValue fSizeRequest
  in case (fams, faces) of
    ([],_)   → pure ∘ Left $ printf "Missing font family '%s'." ffamname
    (_:_,[]) → pure ∘ Left $ printf "No face '%s' in family '%s'." ffacename ffamname
    (_:_,(fFace ∷ GIP.FontFace):_) → do
      fDesc  ← GIP.fontFaceDescribe fFace
      let mayfPISizes = ffacePISizes fFace
          eifSizeFail = case (fSizeRequest, mayfPISizes) of
            (FSROutline fs, Nothing)       → Right fs -- Outline font was requested, and was obtained: we can request any size
            (FSROutline fs, Just fPISizes) →
              if | fPI ∈ fPISizes → Right $ fs
                 | otherwise      → Left  $ printf "Bitmap font family '%s' does not provide for size %s." ffamname (show fs)
            (FSRBitmap   _      _, Nothing) → Left $ printf "Outline font family '%s' does not provide for bitmaps." ffamname
            (FSRBitmap  fs policy, Just fPISizes) →
              case (policy, fPI ∈ fPISizes) of
                (_,  True)  → Right fs
                (EQ, False) → Left failure
                (_,  False) → -- an exact match was unavailable, so let's use the
                              -- policy-provided laxity and seek for a closest one:
                  let (findp, ordered) = if | policy ≡ LT → ((>), sortOn Down fPISizes)
                                            | otherwise   → ((<), sortOn id   fPISizes)
                  in case find (findp fPI) ordered of
                       Nothing → Left failure
                       Just sz → Right $ fromUnit fDΠ sz
              where failure = printf "Bitmap font face '%s' of family '%s' does not have font sizes matching policy %s against size %s, among %s."
                              ffacename ffamname (show policy) (show $ fromPU $ fromUnit fDΠ fPI) (show $ (fromPU ∘ fromUnit fDΠ) <$> fPISizes)
          fontQueryMaxHeight ∷ ∀ m u. (MonadIO m, FromUnit u) ⇒ GIP.Layout → m (He (Unit u))
          fontQueryMaxHeight fDetachedLayout = do
            laySetHeightLimit fDetachedLayout OneLine
            He ∘ (^.di'v._y) <$> layRunTextForSize fDetachedLayout fDΠ (Nothing ∷ Maybe (Wi (Unit u))) "ly"
      case eifSizeFail of
        Left failure → pure $ Left failure
        Right  fSize → do
          fdSetSize fDesc fSize
          fDetachedContext ← GIP.fontMapCreateContext fFontMap
          GIP.contextSetFontDescription    fDetachedContext fDesc
          GIPC.contextSetResolution        fDetachedContext (fromDΠ fDΠ)
          fDetachedLayout ← makeTextLayout fDetachedContext
          fMaxHeight ← fontQueryMaxHeight fDetachedLayout
          pure $ Right $ Font{..}


chooseFont ∷ (MonadIO m) ⇒ FromUnit u ⇒ GIPC.FontMap → [Font Spec u] → m (Maybe (Font Found u), [String])
chooseFont fMap freqs = loop freqs []
  where loop ∷ (MonadIO m) ⇒ FromUnit u ⇒ [Font Spec u] → [String] → m (Maybe (Font Found u), [String])
        loop [] failures  = pure (Nothing, failures)
        loop (fr:rest) fs = validateFont fMap fr
                            >>= (\case
                                    Right font → pure (Just font, fs)
                                    Left  fail → loop rest $ fail:fs)

bindFont ∷ (MonadIO m) ⇒ Font Found u → GIC.Context → m (Font Bound u)
bindFont fbFont@Font{..} gic = do
  fbContext ← GIPC.createContext gic
  GIP.contextSetFontDescription fbContext fDesc
  GIPC.contextSetResolution     fbContext (fromDΠ fDΠ)
  pure FontBinding{..}

-- | 'LayoutHeightLimit', quoting Pango documentation:
--
-- @height: the desired height of the layout in Pango units if positive,
--          or desired number of lines if negative.

-- Sets the height to which the #PangoLayout should be ellipsized at.  There
-- are two different behaviors, based on whether @height is positive or
-- negative.
--
-- If @height is positive, it will be the maximum height of the layout.  Only
-- lines would be shown that would fit, and if there is any text omitted,
-- an ellipsis added.  At least one line is included in each paragraph regardless
-- of how small the height value is.  A value of zero will render exactly one
-- line for the entire layout.
--
-- If @height is negative, it will be the (negative of) maximum number of lines per
-- paragraph.  That is, the total number of lines shown may well be more than
-- this value if the layout contains multiple paragraphs of text.
-- The default value of -1 means that first line of each paragraph is ellipsized.
-- ...
-- Height setting only has effect if a positive width is set on
-- @layout and ellipsization mode of @layout is not %PANGO_ELLIPSIZE_NONE.
-- The behavior is undefined if a height other than -1 is set and
-- ellipsization mode is set to %PANGO_ELLIPSIZE_NONE, and may change in the
-- future.
data LayoutHeightLimit where
  OneLine   ∷ LayoutHeightLimit
  ParaLines ∷ Int → LayoutHeightLimit
  Units     ∷ Unit PUI → LayoutHeightLimit
  deriving (Eq, Show)

instance Monoid LayoutHeightLimit where
  mempty      = OneLine
  mappend l r = choosePartially OneLine l r

data TextSizeSpec u where
  TextSizeSpec ∷ FromUnit u ⇒
    { _tssWidth  ∷ Maybe (Wi (Unit u))
    , _tssHeight ∷ LayoutHeightLimit
    } → TextSizeSpec u

tssWidth  ∷ Lens' (TextSizeSpec u) (Maybe (Wi (Unit u)))
tssWidth  f (TextSizeSpec w h) = flip TextSizeSpec h <$> f w
tssHeight ∷ Lens' (TextSizeSpec u) LayoutHeightLimit
tssHeight f (TextSizeSpec w h) =      TextSizeSpec w <$> f h

fontQuerySize ∷ (HasCallStack, MonadIO m, FromUnit u) ⇒ Font Found u → TextSizeSpec u → Maybe T.Text → m (Either T.Text (Di (Unit u)))
fontQuerySize _ TextSizeSpec{_tssWidth=Nothing} Nothing = pure $ Left "Invariant failed: text size underconstrained."
fontQuerySize Font{..} TextSizeSpec{_tssWidth=Just wi} Nothing =
  pure $ Right $ di wi fMaxHeight
fontQuerySize Font{..} TextSizeSpec{..} (Just text) = do
  laySetHeightLimit fDetachedLayout _tssHeight
  Right <$> layRunTextForSize fDetachedLayout fDΠ _tssWidth text


-- * Text
makeTextLayout ∷ (MonadIO m) ⇒ GIP.Context → m (GIP.Layout)
makeTextLayout gipc = do
  gip ← GIP.layoutNew gipc
  GIP.layoutSetWrap      gip GIP.WrapModeWord
  GIP.layoutSetEllipsize gip GIP.EllipsizeModeEnd
  pure gip

laySetWidth ∷ (MonadIO m) ⇒ FromUnit s ⇒ GIP.Layout → DΠ → Wi (Unit s) → m ()
laySetWidth lay dπ (Wi sz) = do
  let csz = fromPUI $ fromUnit dπ sz
  GIP.layoutSetWidth lay csz

laySetHeight ∷ (MonadIO m) ⇒ FromUnit s ⇒ GIP.Layout → DΠ → Wi (Unit s) → m ()
laySetHeight lay dπ (Wi sz) = do
  let csz = fromPUI $ fromUnit dπ sz
  GIP.layoutSetHeight lay csz

laySetHeightLimit ∷ (MonadIO m) ⇒ GIP.Layout → LayoutHeightLimit → m ()
laySetHeightLimit lay limit = do
  GIP.layoutSetHeight lay $
    case limit of
      OneLine   → 0
      ParaLines x → fromIntegral (-1 ⋅ abs x)
      Units     x → fromIntegral $ fromPUI x

laySetSize ∷ (MonadIO m) ⇒ FromUnit s ⇒ GIP.Layout → DΠ → Di (Unit s) → m ()
laySetSize lay dπ sz = do
  let (Di (V2 cx cy)) = fromPUI ∘ fromUnit dπ <$> sz
  GIP.layoutSetWidth  lay cx
  GIP.layoutSetHeight lay cy

layGetSize ∷ (MonadIO m) ⇒ FromUnit s ⇒ GIP.Layout → DΠ → m (Di (Unit s))
layGetSize lay dπ = do
  (pix, piy) ← GIP.layoutGetPixelSize lay
  pure $ Di $ V2 (fromUnit dπ $ PUs $ fromIntegral pix) (fromUnit dπ $ PUs $ fromIntegral piy)

layPrintLimits ∷ (MonadIO m) ⇒ String → GIP.Layout → m ()
layPrintLimits key lay = do
  w ← GIP.layoutGetWidth  lay
  h ← GIP.layoutGetHeight lay
  liftIO $ printf "-- %s  limw: %s, limh: %s\n" key (show w) (show h)

layRunTextForSize ∷ (MonadIO m) ⇒ (FromUnit s, FromUnit t) ⇒
                    GIP.Layout → DΠ → Maybe (Wi (Unit s)) → Text → m (Di (Unit t))
layRunTextForSize lay dπ mWidth text = do
  case mWidth of
    Nothing    → pure ()
    Just width → laySetWidth lay dπ width
  GIP.layoutSetText lay text (-1)
  sz ← layGetSize        lay dπ
  -- liftIO $ printf "LRTFS '%s' w=%s → %s\n" text (show $ width^.wiV) (show $ sz^.diV)
  pure sz

layDrawText ∷ (MonadIO m) ⇒ Cairo → GIC.Context → GIP.Layout → Po Double → Co Double → T.Text → m ()
layDrawText cairo dGIC lay (Po (V2 cvx cvy)) tColor text =
  runCairo cairo $ do
    GRC.moveTo cvx cvy
    coSetSourceColor tColor
    GIP.layoutSetText lay text (-1)
    GIPC.showLayout dGIC lay


-- | Fontmap: give fonts semantic names.

newtype FontKey
  =          FK { fromFK ∷ T.Text }
  deriving (Eq, Ord, Show, IsString)

newtype FontAlias
  =          Alias { fromAlias ∷ FontKey }
  deriving (Eq, Ord, Show, IsString)

newtype FontPreferences u
  =     FontPreferences [(FontKey, Either FontAlias [Font Spec u])]
  deriving (Eq, Show)

data FontMap u where
  FontMap ∷
    { fmDΠ    ∷ DΠ
    , fmFonts ∷ (Map FontKey (Font Found u))
    } → FontMap u
deriving instance Show (FontMap u)

makeFontMap ∷ (MonadIO m) ⇒ FromUnit u ⇒ HasCallStack ⇒ DΠ → GIPC.FontMap → FontPreferences u → m (FontMap u)
makeFontMap dπ gipcFM (FontPreferences prefsAndAliases) =
                         foldM resolvePrefs Map.empty ((id *** fromRight (⊥)) <$> prefs)
  <&> FontMap dπ ∘ flip (foldl resolveAlias)          ((id *** fromLeft  (⊥)) <$> aliases)
  where resolvePrefs acc (fkey, freqs) = do
          (mFont, errs) ← chooseFont gipcFM freqs
          let font = mFont &
                fromMaybe (error $ printf "FATAL: while searching font for slot '%s', no suitable font among: %s.  Failures: %s" (show fkey) (show freqs) (show errs))
          pure $ Map.insert fkey font acc
        resolveAlias fontmap (fk, (Alias alias)) = flip (Map.insert fk) fontmap
                                                   $ Map.lookup alias fontmap
                                                   & flip fromMaybe
                                                   $ error $ printf "ERROR: while resolving alias for slot '%s', no slot named '%s' exists."
                                                             (T.unpack $ fromFK fk) (T.unpack $ fromFK alias)
        (aliases, prefs) = flip partition prefsAndAliases
                           (\(_, aEp) → isLeft aEp)

lookupFont ∷ FontMap u → FontKey → Maybe (Font Found u)
lookupFont (FontMap _ fm) fk = Map.lookup fk fm

lookupFont' ∷ FontMap u → FontKey → Font Found u
lookupFont' fm fk = lookupFont fm fk
                    & fromMaybe (error $ printf "ERROR: unexpected missing fontkey '%s'." (show fk))
