{-# LANGUAGE DataKinds, GADTs, KindSignatures, TypeFamilies #-}
{-# LANGUAGE DeriveFunctor, GeneralizedNewtypeDeriving, StandaloneDeriving #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase, MultiWayIf, OverloadedStrings, RecordWildCards, ScopedTypeVariables, TupleSections #-}
{-# LANGUAGE UnicodeSyntax #-}

module HoloFont where

-- Basis
import           Prelude.Unicode
import           Control.Lens

-- Types
import           Control.Monad                            (unless, when, forM_, filterM)
import           Control.Monad.Trans.Reader               (ReaderT(..))
import qualified Data.ByteString.Char8             as SB
import qualified Data.ByteString.Lazy              as LB
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

newtype PPI = PPI { ppiVal ∷ Double } deriving (Num, Show)
ppi ∷ PPI
ppi = 72

newtype DPI = DPI { fromDPI ∷ Double } deriving (Num, Show)
-- ^ Pango fontmap resolution:
--   > Sets the resolution for the fontmap. This is a scale factor between
--   > points specified in a #PangoFontDescription and Cairo units. The
--   > default value is 96, meaning that a 10 point font will be 13
--   > units high. (10 * 96. / 72. = 13.3).
--   cited from: https://git.gnome.org/browse/pango/tree/pango/pangocairo-fontmap.c#n236
--   I.e.:
--     units  = points ⋅ dpi / ppi
--     points = units  / dpi ⋅ ppi
--
--   A good situation report is also at:
--     https://mail.gnome.org/archives/gtk-i18n-list/2002-May/msg00004.html
--
-- double pango_units_to_double   (int i)    { return (double)i / PANGO_SCALE; }
-- int    pango_units_from_double (double d) { return (int)floor (d * PANGO_SCALE + 0.5); }

toPU ∷ DPI → Size a → Size PU
toPU _ x@(PUs _)         = x
toPU (DPI dpi) (Pts pts) = PUs $ (fromIntegral pts) ⋅ dpi / ppiVal ppi
toPt ∷ DPI → Size a → Size Pt
toPt _ x@(Pts _)         = x
toPt (DPI dpi) (PUs pus) = Pts $ floor $ pus ⋅ ppiVal ppi / dpi
sizeCoerce ∷ DPI → Size b → Size a → Size b
sizeCoerce _      (PUs _) x@(PUs _) = x
sizeCoerce _      (Pts _) x@(Pts _) = x
sizeCoerce dpi to@(Pts _) x@(PUs _) = toPt dpi x
sizeCoerce dpi to@(PUs _) x@(Pts _) = toPU dpi x

-- pcResolution ∷ GIP.Context → IO DPI
-- pcResolution ctx = DPI <$> GIPC.contextGetResolution ctx
-- pcSetResolution ∷ GIP.Context → DPI → IO ()
-- pcSetResolution ctx = GIPC.contextSetResolution ctx ∘ fromDPI

-- | Get fontmap resolution.
-- This function isn't in IO, because the resolution mutation function isn't defined.
-- If ever something is exported that changes the fontmap resolution, this function's
-- signature must be moved to IO.
fmResolution ∷ GIPC.FontMap → DPI
fmResolution = DPI ∘ UN.unsafePerformIO ∘ GIPC.fontMapGetResolution
-- fmSetResolution ∷ GIPC.FontMap → DPI → IO ()
-- fmSetResolution fm = GIPC.fontMapSetResolution fm ∘ fromDPI

-- Note [Fontmap type]
-- ~~~~~~~~~~~~~~~~~~~
-- We choose to entirely avoid GI.Pango.FontMap,
-- because it has no intrinsic concept of resolution,
-- which we heavily rely upon.
-- Hence, we opt to GI.PangoCairo.FontMap everywhere.
--
instance Show GIPC.FontMap where
  show = printf "FontMap { resolution = %fDPI }" ∘ fromDPI ∘ fmResolution
instance Show GIP.FontFamily where
  show = printf "FontFamily { name = '%s' }" ∘ ffamName
instance Show GIP.FontFace where
  show = printf "FontFace { name = '%s' }" ∘ ffacName

fmDefault ∷ GIPC.FontMap
fmDefault = (GI.unsafeCastTo GIPC.FontMap =<< GIPC.fontMapGetDefault)
            & UN.unsafePerformIO
fmFamilies ∷ GIP.IsFontMap a ⇒ a → [GIP.FontFamily]
fmFamilies = UN.unsafePerformIO ∘ GIP.fontMapListFamilies

ffamName ∷ GIP.FontFamily → Text
ffamName = UN.unsafePerformIO ∘ GIP.fontFamilyGetName
ffamFaces ∷ GIP.FontFamily → [GIP.FontFace]
ffamFaces = UN.unsafePerformIO ∘ GIP.fontFamilyListFaces

ffacName ∷ GIP.FontFace → Text
ffacName = UN.unsafePerformIO ∘ GIP.fontFaceGetFaceName
ffacPUSizes ∷ GIP.FontFace → Maybe [Size PU]
ffacPUSizes fface = (fromIntegral <$>) <$> (UN.unsafePerformIO (GIP.fontFaceListSizes fface))


-- * Font sizes & resolution
data SizeKind = PU | Pt
data Size (a ∷ SizeKind) where
  PUs ∷ { fromPU ∷ !Double }  → Size PU -- ^ Pango font size, in device units
  Pts ∷ { fromPt ∷ !F.Int32 } → Size Pt -- ^ Pango font size, in points (at 72ppi rate), device-agnostic

-- <Boilerplate>
deriving instance Eq   (Size a)
deriving instance Ord  (Size a)
deriving instance Show (Size a)
type instance Element (Size PU) = Double
type instance Element (Size Pt) = F.Int32
instance MonoFunctor (Size a) where
  omap f (PUs x) = PUs (f x)
  omap f (Pts x) = Pts (f x)
instance Num (Size PU) where
  fromInteger = PUs ∘ UN.unsafePerformIO ∘ GIP.unitsToDouble ∘ fromIntegral; PUs x + PUs y = PUs $ x + y; PUs x * PUs y = PUs $ x * y
  abs = omap abs; signum = omap signum; negate = omap negate
instance Num (Size Pt) where
  fromInteger = Pts ∘ fromIntegral;                                          Pts x + Pts y = Pts $ x + y; Pts x * Pts y = Pts $ x * y
  abs = omap abs; signum = omap signum; negate = omap negate
-- </Boilerplate>

fdSetSize ∷ GIP.FontDescription → Size a → IO ()
fdSetSize fd (PUs us)  = GIP.fontDescriptionSetAbsoluteSize fd us
fdSetSize fd (Pts pts) = GIP.fontDescriptionSetSize         fd pts

--- XXX/Pipedream: interchangeable font sizes unsustainable due to necessity of font scale context
-- newtype PUss     = PUss     { fromPUss     ∷ Double }  deriving (Num, Show)
-- newtype FSPoints  = FSPoints  { fromFSPoints  ∷ F.Int32 } deriving (Num, Show)
-- instance SizeValue a ⇒ SizeValue (SizeRequest a) where
--   fsPUs    = fsPUs    ∘ fsValue
--   fsPoints = fsPoints ∘ fsValue
--   fdSetSize = \fd → fdSetSize fd ∘ fsValue

data SizeRequest a where
  FSROutline ∷
    { fsValue   ∷ Size a
    } → SizeRequest a
  FSRBitmap  ∷
    { fsValue   ∷ Size a
    , fsbPolicy ∷ Ordering
    } → SizeRequest a
deriving instance Show (SizeRequest a)

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

data Font (valid ∷ Bool) (a ∷ SizeKind) where
  FontReq ∷
    { frFamilyName  ∷ FamilyName
    , frFaceName    ∷ FaceName
    , frSizeRequest ∷ SizeRequest a
    } → Font False a
  Font ∷
    { fFamilyName ∷ FamilyName
    , fFaceName   ∷ FaceName
    , fFace       ∷ GIP.FontFace
    , fSize       ∷ Size a
    , fDesc       ∷ GIP.FontDescription
    } → Font True a
instance Show (Font True a) where
  show Font{..} =
    printf "Font { family = %s, face = %s, size = %s }"
    (fromFamilyName fFamilyName) (fromFaceName fFaceName) (show fSize)

validateFont ∷ GIPC.FontMap → Font False a → IO (Either String (Font True a))
validateFont fMap (FontReq
                   fFamilyName@(FamilyName ffamname)
                   fFaceName@(FaceName ffacename)
                   fSizeRequest) =
  let fams  = filter ((≡ ffamname)  ∘ ffamName) $ fmFamilies fMap
      faces = filter ((≡ ffacename) ∘ ffacName) $ concat $ ffamFaces <$> fams
      dpi   = fmResolution fMap
      fPU   = toPU dpi $ fsValue fSizeRequest
  in case (fams, faces) of
    ([],_)           → pure ∘ Left $ printf "Missing font family '%s'." ffamname
    (ffam:_,[])      → pure ∘ Left $ printf "No face '%s' in family '%s'." ffacename ffamname
    (ffam:_,(fFace ∷ GIP.FontFace):_) → do
      fDesc  ← GIP.fontFaceDescribe fFace
      let mayfSizes = ffacPUSizes fFace
          eifSizeFail = case (fSizeRequest, mayfSizes) of
            (FSROutline fs, Nothing)       → Right fs -- Outline font was requested, and was obtained: we can request any size
            (FSROutline fs, Just fPUSizes) →
              if | fPU ∈ fPUSizes → Right $ fs
                 | otherwise      → Left  $ printf "Bitmap font family '%s' does not provide for size %s." ffamname (show fs)
            (FSRBitmap  fs      _, Nothing) → Left $ printf "Outline font family '%s' does not provide for bitmaps." ffamname
            (FSRBitmap  fs policy, Just fPUSizes) →
              case (policy, fPU ∈ fPUSizes) of
                (_,  True)  → Right fs
                (EQ, False) → Left failure
                (_,  False) → -- an exact match was unavailable, so let's use the
                              -- policy-provided laxity and seek for a closest one:
                  let (findp, ordered) = if | policy ≡ LT → ((>), sortOn Down fPUSizes)
                                            | otherwise   → ((<), sortOn id   fPUSizes)
                  in case find (findp fPU) ordered of
                       Nothing → Left failure
                       Just sz → Right $ sizeCoerce dpi (fsValue fSizeRequest) sz
              where failure = printf "Bitmap font face '%s' of family '%s' does not have font sizes matching policy %s against size %s, among %s."
                              ffacename ffamname (show policy) (show $ fromPU fPU) (show $ fromPU <$> fPUSizes)
      case eifSizeFail of
        Left failure → pure $ Left failure
        Right  fSize → do
          fdSetSize fDesc fSize
          pure $ Right $ Font{..}

chooseFont ∷ GIPC.FontMap → [Font False a] → IO (Maybe (Font True a), [String])
chooseFont fMap freqs = loop freqs []
  where loop ∷ [Font False a] → [String] → IO (Maybe (Font True a), [String])
        loop [] failures  = pure (Nothing, failures)
        loop (fr:rest) fs = validateFont fMap fr
                            >>= (\case
                                    Right font → pure (Just font, fs)
                                    Left  fail → loop rest $ fail:fs)


defaultFontDesc, terminusFontDesc, aurulentFontDesc ∷ GIP.FontDescription
aurulentFontDesc = UN.unsafePerformIO $ fontDescriptionFromArgs "Aurulent Sans" GIP.StyleNormal 12288
terminusFontDesc = UN.unsafePerformIO $ fontDescriptionFromArgs "Terminus"      GIP.StyleNormal 12288
defaultFontDesc = terminusFontDesc -- aurulentFontDesc

fontDescriptionFromArgs ∷ String → GIP.Style → Int → IO GIP.FontDescription
fontDescriptionFromArgs family style pus = do
  fd ← GIP.fontDescriptionNew
  GIP.fontDescriptionSetFamily fd $ T.pack family
  GIP.fontDescriptionSetStyle  fd style
  GIP.fontDescriptionSetSize   fd $ fromIntegral pus
  pure fd
