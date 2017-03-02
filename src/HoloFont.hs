{-# LANGUAGE DataKinds, GADTs, KindSignatures, TypeFamilies #-}
{-# LANGUAGE DeriveFunctor, GeneralizedNewtypeDeriving, StandaloneDeriving #-}
{-# LANGUAGE FlexibleContexts, FlexibleInstances #-}
{-# LANGUAGE LambdaCase, MultiWayIf, OverloadedStrings, RecordWildCards, ScopedTypeVariables, TupleSections #-}
{-# LANGUAGE UnicodeSyntax #-}

module HoloFont
  ( Font(..)
  , FontSizeRequest(..)
  , FamilyName, FaceName

  -- * Query
  , fmDefault, fmFamilies, fmResolution
  , ffamName, ffamFaces
  , ffacName, ffacPISizes

  -- * Main API
  , validateFont, chooseFont

  -- * Ancillary
  , fdSetSize

  -- * Text
  , KTextSettings(..), TextSettings(..), makeTextSettings, makeTextContext
  , makeTextLayout, laySetWidth, layGetSize, layRunTextForSize

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
fdSetSize ∷ GIP.FontDescription → Size u → IO ()
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

data Font (valid ∷ Bool) (u ∷ KUnit) where
  FontReq ∷
    { frFamilyName  ∷ FamilyName
    , frFaceName    ∷ FaceName
    , frSizeRequest ∷ FontSizeRequest u
    } → Font False u
  Font ∷
    { fFamilyName ∷ FamilyName
    , fFaceName   ∷ FaceName
    , fFace       ∷ GIP.FontFace
    , fSize       ∷ Size u
    , fDesc       ∷ GIP.FontDescription
    } → Font True u
instance Show (Font False u) where
  show FontReq{..} =
    printf "FontReq { family = %s, face = %s, size = %s }"
    (fromFamilyName frFamilyName) (fromFaceName frFaceName) (show frSizeRequest)
instance Show (Font True u) where
  show Font{..} =
    printf "Font { family = %s, face = %s, size = %s }"
    (fromFamilyName fFamilyName) (fromFaceName fFaceName) (show fSize)

validateFont ∷ Sizely (Size u) ⇒ GIPC.FontMap → Font False u → IO (Either String (Font True u))
validateFont fMap (FontReq
                   fFamilyName@(FamilyName ffamname)
                   fFaceName@(FaceName ffacename)
                   fSizeRequest) =
  let fams  = filter ((≡ ffamname)  ∘ ffamName) $ fmFamilies fMap
      faces = filter ((≡ ffacename) ∘ ffacName) $ concat $ ffamFaces <$> fams
      dπ   = fmResolution fMap
      fPI   = fromSz dπ $ fsValue fSizeRequest
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
                       Just sz → Right $ fromSz dπ sz
              where failure = printf "Bitmap font face '%s' of family '%s' does not have font sizes matching policy %s against size %s, among %s."
                              ffacename ffamname (show policy) (show $ fromPU $ fromSz dπ fPI) (show $ (fromPU ∘ fromSz dπ) <$> fPISizes)
      case eifSizeFail of
        Left failure → pure $ Left failure
        Right  fSize → do
          fdSetSize fDesc fSize
          pure $ Right $ Font{..}

chooseFont ∷ Sizely (Size u) ⇒ GIPC.FontMap → [Font False u] → IO (Maybe (Font True u), [String])
chooseFont fMap freqs = loop freqs []
  where loop ∷ Sizely (Size u) ⇒ [Font False u] → [String] → IO (Maybe (Font True u), [String])
        loop [] failures  = pure (Nothing, failures)
        loop (fr:rest) fs = validateFont fMap fr
                            >>= (\case
                                    Right font → pure (Just font, fs)
                                    Left  fail → loop rest $ fail:fs)


-- * Text
data KTextSettings
  = TSProto
  | TSPhys

data TextSettings (k ∷ KTextSettings) (u ∷ KUnit) where
  TextSettings ∷
    { tsFontMap  ∷ GIP.FontMap
    , tsDΠ       ∷ DΠ
    , tsFont     ∷ Font True u
    , tsDetached ∷ GIP.Context
    , tsLayout   ∷ GIP.Layout
    } → TextSettings TSProto u
  TextContext ∷
    { tsProto    ∷ TextSettings TSProto u
    , tsPhysical ∷ GIP.Context
    } → TextSettings TSPhys u
instance Show (TextSettings k u) where
  show              (TextSettings _ dπ font _ _)    = printf "TextSettings { dpi = %f, font = %s }" (show dπ) (show font)
  show (TextContext (TextSettings _ dπ font _ _) _) = printf  "TextContext { dpi = %f, font = %s }" (show dπ) (show font)

tsContext ∷ TextSettings k u → GIP.Context
tsContext TextSettings{..} = tsDetached
tsContext TextContext{..}  = tsPhysical

makeTextSettings ∷ GIP.FontMap → DΠ → Font True u → IO (TextSettings TSProto u)
makeTextSettings tsFontMap tsDΠ@(DΠ dπ) tsFont@Font{..} = do
  tsDetached ← GIP.fontMapCreateContext tsFontMap
  GIP.contextSetFontDescription tsDetached fDesc
  GIPC.contextSetResolution     tsDetached dπ
  let ts = TextSettings{..}
  tsLayout ← makeTextLayout ts
  pure ts { tsLayout = tsLayout }

makeTextContext ∷ TextSettings TSProto u → GIC.Context → IO (TextSettings TSPhys u)
makeTextContext tsProto@TextSettings{..} gic = do
  tsPhysical ← GIPC.createContext gic
  GIP.contextSetFontDescription tsPhysical (fDesc tsFont)
  GIPC.contextSetResolution     tsPhysical (fromDΠ tsDΠ)
  pure TextContext{..}

makeTextLayout ∷ TextSettings k u → IO (GIP.Layout)
makeTextLayout ts = do
  gip ← GIP.layoutNew $ tsContext ts
  GIP.layoutSetWrap      gip GIP.WrapModeWord
  GIP.layoutSetEllipsize gip GIP.EllipsizeModeEnd
  pure gip

laySetWidth ∷ Sizely (Size s) ⇒
              GIP.Layout → DΠ → Wi (Size s) → IO ()
laySetWidth lay dπ (Wi sz) =
  GIP.layoutSetWidth lay ∘ fromPUI $ fromSz dπ sz

layGetSize ∷ Sizely (Size s) ⇒
             GIP.Layout → DΠ → IO (Di (Size s))
layGetSize lay dπ = do
  (pix, piy) ← GIP.layoutGetPixelSize lay
  t ← GIP.layoutGetText lay
  pure $ Di $ V2 (fromSz dπ $ PUs $ fromIntegral pix) (fromSz dπ $ PUs $ fromIntegral piy)

layRunTextForSize ∷ (Sizely (Size s), Sizely (Size t)) ⇒
                    GIP.Layout → DΠ → Wi (Size s) → Text → IO (Di (Size t))
layRunTextForSize lay dπ width text = do
  laySetWidth       lay dπ width
  GIP.layoutSetText lay text (-1)
  layGetSize        lay dπ


-- | Fontmap: give fonts semantic names.

newtype FontKey
  =          FK { fromFK ∷ T.Text }
  deriving (Eq, Ord, Show, IsString)

newtype FontAlias
  =          Alias { fromAlias ∷ FontKey }
  deriving (Eq, Ord, Show, IsString)

newtype FontPreferences u
  =     FontPreferences [(FontKey, Either FontAlias [Font False u])]

data FontMap u where
  FontMap ∷
    { fmDΠ    ∷ DΠ
    , fmFonts ∷ (Map FontKey (Font True u))
    } → FontMap u

makeFontMap ∷ Sizely (Size u) ⇒ HasCallStack ⇒ DΠ → GIPC.FontMap → FontPreferences u → IO (FontMap u)
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

lookupFont ∷ FontMap u → FontKey → Maybe (Font True u)
lookupFont (FontMap _ fm) fk = Map.lookup fk fm

lookupFont' ∷ FontMap u → FontKey → Font True u
lookupFont' fm fk = lookupFont fm fk
                    & fromMaybe (error $ printf "ERROR: unexpected missing fontkey '%s'." (show fk))
