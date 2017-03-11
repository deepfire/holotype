{-# LANGUAGE DataKinds, GADTs, KindSignatures, TypeFamilies #-}
{-# LANGUAGE DeriveFunctor, GeneralizedNewtypeDeriving, StandaloneDeriving #-}
{-# LANGUAGE FlexibleContexts, FlexibleInstances #-}
{-# LANGUAGE LambdaCase, MultiWayIf, OverloadedStrings, RecordWildCards, ScopedTypeVariables, TupleSections #-}
{-# LANGUAGE UnicodeSyntax #-}

module HoloFont
  ( Font(..), FKind(..)
  , FontSizeRequest(..)
  , FamilyName, FaceName

  -- * Query
  , fmDefault, fmFamilies, fmResolution
  , ffamName, ffamFaces
  , ffacName, ffacPISizes

  -- * Main API
  , validateFont, chooseFont, bindFont

  -- * Ancillary
  , fdSetSize

  -- * Text
  , makeTextLayout, laySetWidth, laySetHeight, laySetSize, layGetSize, laySetMaxParaLines
  , layPrintLimits , layRunTextForSize
  , layDrawText

  -- * FontMap
  , FontKey(..), FontAlias(..), FontPreferences(..), FontMap(..)
  , makeFontMap, lookupFont, lookupFont'
  )
where

-- Basis
import           Prelude.Unicode
import           Control.Arrow
import           Control.Lens
import           GHC.Stack

-- Types
import           Control.Monad                            (unless, when, forM_, foldM, filterM)
import           Control.Monad.IO.Class                   (MonadIO, liftIO)
import           Control.Monad.Trans.Reader               (ReaderT(..))
import qualified Data.ByteString.Char8             as SB
import qualified Data.ByteString.Lazy              as LB
import           Data.Either
import           Data.Either.Extra
import           Data.List
import           Data.Map                                 (Map)
import           Data.Ord
import qualified Data.Map                          as Map
import           Data.Maybe                               (fromMaybe)
import           Data.MonoTraversable
import           Data.String                              (IsString)
import           Data.Text                         as T   (Text, pack, unpack)
import qualified Data.Vector                       as V
import qualified Data.Vect                         as Vc
import           Data.Vect                                (Mat4(..), Vec3(..), Vec4(..))
import           Numeric.Extra                            (floatToDouble, doubleToFloat)

-- Algebra
import           Linear

-- Misc
import           System.FilePath                          ((</>))
import qualified System.Directory                  as FS
import           Text.Show.Pretty                         (ppShow)
import           Text.Printf                              (printf)

-- Manually-bound Cairo
import qualified Graphics.Rendering.Cairo          as GRC
import qualified Graphics.Rendering.Cairo.Internal as GRC (Render(..), create, imageSurfaceCreate)
import qualified Graphics.Rendering.Cairo.Types    as GRC
import qualified Graphics.Rendering.Cairo.Internal as GRCI

-- glib-introspection -based Cairo and Pango
import qualified Data.GI.Base                      as GI
import qualified GI.Cairo                          as GIC
import qualified GI.Cairo.Structs.Context          as GIC
import qualified GI.Pango                          as GIP
import qualified GI.PangoCairo.Interfaces.FontMap  as GIPC
import qualified GI.PangoCairo.Functions           as GIPC

-- Dirty stuff
import qualified Foreign.C.Types                   as F
import qualified Foreign                           as F
import qualified Foreign.Ptr                       as F
import qualified System.IO.Unsafe                  as UN

-- Local imports
import Flatland


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
  show = printf "FontFace { name = '%s' }" ∘ ffacName

class PangoFontMap a where
  fmDefault ∷ a
instance PangoFontMap GIP.FontMap where
  fmDefault = GIPC.fontMapGetDefault & UN.unsafePerformIO
instance PangoFontMap GIPC.FontMap where
  fmDefault = (GI.unsafeCastTo GIPC.FontMap =<< GIPC.fontMapGetDefault) & UN.unsafePerformIO

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

ffacName ∷ GIP.FontFace → Text
ffacName = UN.unsafePerformIO ∘ GIP.fontFaceGetFaceName
ffacPISizes ∷ GIP.FontFace → Maybe [Size PUI]
ffacPISizes fface = (fromIntegral <$>) <$> (UN.unsafePerformIO (GIP.fontFaceListSizes fface))


-- * Font sizes

-- | Set font size: XXX/upstream/inconsistency -- Double, yet PANGO_SCALE-d:
--   https://hackage.haskell.org/package/gi-pango-1.0.11/docs/GI-Pango-Structs-FontDescription.html#v:fontDescriptionSetAbsoluteSize
--   https://hackage.haskell.org/package/gi-pango-1.0.11/docs/GI-Pango-Structs-FontDescription.html#v:fontDescriptionSetSize
fdSetSize ∷ (MonadIO m) ⇒ GIP.FontDescription → Size u → m ()
fdSetSize fd (PUs  us)  = GIP.fontDescriptionSetAbsoluteSize fd $ us * fromIntegral GIP.SCALE
fdSetSize fd (PUIs is)  = GIP.fontDescriptionSetAbsoluteSize fd $ fromIntegral is
fdSetSize fd (Pts  pts) = GIP.fontDescriptionSetSize         fd $ pts * GIP.SCALE

data FontSizeRequest u where
  FSROutline ∷ Sizely (Size u) ⇒
    { fsValue   ∷ Size u
    } → FontSizeRequest u
  FSRBitmap  ∷ Sizely (Size u) ⇒
    { fsValue   ∷ Size u
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
    , fSize            ∷ Size u
    , fDesc            ∷ GIP.FontDescription
    , fFontMap         ∷ GIPC.FontMap
    , fDΠ              ∷ DΠ
    , fDetachedContext ∷ GIP.Context
    , fDetachedLayout  ∷ GIP.Layout
    } → Font Found u
  FontBinding ∷
    { fbFont           ∷ Font Found u
    , fbContext        ∷ GIP.Context
    } → Font Bound u

instance Show (Font Spec u) where
  show FontSpec{..} =
    printf "FontSpec { family = %s, face = %s, size = %s }"
    (fromFamilyName frFamilyName) (fromFaceName frFaceName) (show frSizeRequest)
instance Show (Font Found u) where
  show Font{..} =
    printf "Font { family = %s, face = %s, size = %s }"
    (fromFamilyName fFamilyName) (fromFaceName fFaceName) (show fSize)

validateFont ∷ (MonadIO m) ⇒ Sizely (Size u) ⇒ GIPC.FontMap → Font Spec u → m (Either String (Font Found u))
validateFont fFontMap (FontSpec
                       fFamilyName@(FamilyName ffamname)
                       fFaceName@(FaceName ffacename)
                       fSizeRequest) =
  let fams  = filter ((≡ ffamname)  ∘ ffamName) $ fmFamilies fFontMap
      faces = filter ((≡ ffacename) ∘ ffacName) $ concat $ ffamFaces <$> fams
      fDΠ   = fmResolution fFontMap
      fPI   = fromSz fDΠ $ fsValue fSizeRequest
  in case (fams, faces) of
    ([],_)           → pure ∘ Left $ printf "Missing font family '%s'." ffamname
    (ffam:_,[])      → pure ∘ Left $ printf "No face '%s' in family '%s'." ffacename ffamname
    (ffam:_,(fFace ∷ GIP.FontFace):_) → do
      fDesc  ← GIP.fontFaceDescribe fFace
      let mayfPISizes = ffacPISizes fFace
          eifSizeFail = case (fSizeRequest, mayfPISizes) of
            (FSROutline fs, Nothing)       → Right fs -- Outline font was requested, and was obtained: we can request any size
            (FSROutline fs, Just fPISizes) →
              if | fPI ∈ fPISizes → Right $ fs
                 | otherwise      → Left  $ printf "Bitmap font family '%s' does not provide for size %s." ffamname (show fs)
            (FSRBitmap  fs      _, Nothing) → Left $ printf "Outline font family '%s' does not provide for bitmaps." ffamname
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
                       Just sz → Right $ fromSz fDΠ sz
              where failure = printf "Bitmap font face '%s' of family '%s' does not have font sizes matching policy %s against size %s, among %s."
                              ffacename ffamname (show policy) (show $ fromPU $ fromSz fDΠ fPI) (show $ (fromPU ∘ fromSz fDΠ) <$> fPISizes)
      case eifSizeFail of
        Left failure → pure $ Left failure
        Right  fSize → do
          fdSetSize fDesc fSize
          fDetachedContext ← GIP.fontMapCreateContext fFontMap
          GIP.contextSetFontDescription    fDetachedContext fDesc
          GIPC.contextSetResolution        fDetachedContext (fromDΠ fDΠ)
          fDetachedLayout ← makeTextLayout fDetachedContext
          pure $ Right $ Font{..}

chooseFont ∷ (MonadIO m) ⇒ Sizely (Size u) ⇒ GIPC.FontMap → [Font Spec u] → m (Maybe (Font Found u), [String])
chooseFont fMap freqs = loop freqs []
  where loop ∷ (MonadIO m) ⇒ Sizely (Size u) ⇒ [Font Spec u] → [String] → m (Maybe (Font Found u), [String])
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


-- * Text
makeTextLayout ∷ (MonadIO m) ⇒ GIP.Context → m (GIP.Layout)
makeTextLayout gipc = do
  gip ← GIP.layoutNew gipc
  GIP.layoutSetWrap      gip GIP.WrapModeWord
  GIP.layoutSetEllipsize gip GIP.EllipsizeModeEnd
  pure gip

laySetWidth ∷ (MonadIO m) ⇒ Sizely (Size s) ⇒ GIP.Layout → DΠ → Wi (Size s) → m ()
laySetWidth lay dπ (Wi sz) = do
  let csz = fromPUI $ fromSz dπ sz
  GIP.layoutSetWidth lay csz

laySetHeight ∷ (MonadIO m) ⇒ Sizely (Size s) ⇒ GIP.Layout → DΠ → Wi (Size s) → m ()
laySetHeight lay dπ (Wi sz) = do
  let csz = fromPUI $ fromSz dπ sz
  GIP.layoutSetHeight lay csz

laySetMaxParaLines ∷ (MonadIO m) ⇒ GIP.Layout → Int → m ()
laySetMaxParaLines lay maxParaLines = do
  GIP.layoutSetHeight lay $ fromIntegral (-1 ⋅ abs maxParaLines)

laySetSize ∷ (MonadIO m) ⇒ Sizely (Size s) ⇒ GIP.Layout → DΠ → Di (Size s) → m ()
laySetSize lay dπ sz = do
  let (Di (V2 cx cy)) = fromPUI ∘ fromSz dπ <$> sz
  GIP.layoutSetWidth  lay cx
  GIP.layoutSetHeight lay cy

layGetSize ∷ (MonadIO m) ⇒ Sizely (Size s) ⇒ GIP.Layout → DΠ → m (Di (Size s))
layGetSize lay dπ = do
  (pix, piy) ← GIP.layoutGetPixelSize lay
  pure $ Di $ V2 (fromSz dπ $ PUs $ fromIntegral pix) (fromSz dπ $ PUs $ fromIntegral piy)

layPrintLimits ∷ (MonadIO m) ⇒ String → GIP.Layout → m ()
layPrintLimits key lay = do
  w ← GIP.layoutGetWidth  lay
  h ← GIP.layoutGetHeight lay
  GRC.liftIO $ printf "-- %s  limw: %s, limh: %s\n" key (show w) (show h)

layRunTextForSize ∷ (MonadIO m) ⇒ (Sizely (Size s), Sizely (Size t)) ⇒
                    GIP.Layout → DΠ → Wi (Size s) → Text → m (Di (Size t))
layRunTextForSize lay dπ width text = do
  laySetWidth       lay dπ width
  GIP.layoutSetText lay text (-1)
  layGetSize        lay dπ

layDrawText ∷ (MonadIO m) ⇒ GRC.Cairo → GIC.Context → GIP.Layout → Po Double → Co Double → T.Text → m ()
layDrawText dGRC dGIC lay (Po (V2 cvx cvy)) tColor text = do
  liftIO $ (`runReaderT` dGRC) $ GRC.runRender $ do
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

data FontMap u where
  FontMap ∷
    { fmDΠ    ∷ DΠ
    , fmFonts ∷ (Map FontKey (Font Found u))
    } → FontMap u
deriving instance Show (FontMap u)

makeFontMap ∷ (MonadIO m) ⇒ Sizely (Size u) ⇒ HasCallStack ⇒ DΠ → GIPC.FontMap → FontPreferences u → m (FontMap u)
makeFontMap dπ gipcFM (FontPreferences prefsAndAliases) =
                         foldM resolvePrefs Map.empty ((id *** fromRight) <$> prefs)
  <&> FontMap dπ ∘ flip (foldl resolveAlias)          ((id *** fromLeft)  <$> aliases)
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
