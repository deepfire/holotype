{-# LANGUAGE GADTs, TypeFamilies, TypeFamilyDependencies, TypeInType #-}
{-# LANGUAGE GeneralizedNewtypeDeriving, StandaloneDeriving #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE ExplicitForAll, FlexibleContexts, FlexibleInstances, MultiParamTypeClasses, RankNTypes, UndecidableInstances #-}
{-# LANGUAGE LambdaCase, OverloadedStrings, PartialTypeSignatures, RecordWildCards, ScopedTypeVariables, TupleSections, TypeOperators #-}
{-# LANGUAGE UnicodeSyntax #-}
{-# OPTIONS_GHC -Wall -Wno-unticked-promoted-constructors #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}

module HoloCanvas where

-- Basis
import           Prelude                           hiding ((.))
import           Prelude.Unicode
import           Control.Applicative.Free
import           Control.Lens

-- Type-level
import           GHC.Types
import           GHC.TypeLits                      hiding (Text)

-- Types
import           Control.Monad                            (when, forM_)
import           Control.Monad.IO.Class                   (MonadIO, liftIO)
import qualified Data.Map                          as Map
import qualified Data.Text                         as T
import qualified Data.Vector                       as V
import qualified Data.Vect                         as Vc
import           Data.Vect                                (Mat4(..), Vec3(..))
import           Numeric.Extra                            (doubleToFloat)

-- Algebra
import           Linear

-- Manually-bound Cairo
import qualified Graphics.Rendering.Cairo          as GRC
import qualified Graphics.Rendering.Cairo.Internal as GRCI

-- glib-introspection -based Cairo and Pango
import qualified GI.Cairo                          as GIC
import qualified GI.Pango                          as GIP

-- Dirty stuff
import qualified Data.IORef                        as IO
import qualified Foreign.C.Types                   as F
import qualified Foreign                           as F
import qualified System.Mem.Weak                   as SMem
import qualified System.IO.Unsafe                  as XXX

-- …
import           Graphics.GL.Core33                as GL

-- LambdaCube
import qualified LambdaCube.GL                     as GL
import qualified LambdaCube.GL.Mesh                as GL
import qualified LambdaCube.Linear                 as LCLin
import           LambdaCube.Mesh                   as LC

-- LambdaCube Quake III
import           GameEngine.Utils                  as Q3

-- Local imports
import Elsewhere
import Flatland
import FlatDraw
import Flex
import qualified HoloCairo                         as Cr
import qualified HoloCube                          as HC


-- * Very early generic widget code.
-- type DrawableSpace p d = Space             p Double d
-- type WidgetSpace     d = DrawableSpace False        d

-- class Show (StyleOf a) ⇒ Element a where
--   type StyleOf a = (r ∷ Type) | r → a
--   type Content a ∷ Type
--   type Depth   a ∷ Nat

-- class Element w ⇒ Widget w where
  -- | Query size: style meets content → compute spatial parameters.
  -- query          ∷ (MonadIO m) ⇒ Settings PU           → StyleOf w → Content w → m (DrawableSpace False (Depth w))
  -- -- | Add target and space: given a drawable and a pinned space, prepare for 'render'.
  -- make           ∷ (MonadIO m) ⇒ Settings PU → CanvasW → StyleOf w → Content w →    DrawableSpace True  (Depth w) → m w
  -- -- | Per-content-change: mutate pixels of the bound drawable.
  -- draw           ∷ (MonadIO m) ⇒ CanvasW → w → m ()

-- class   Element w ⇒ Container w where
--   type   Inner w ∷ Type
--   innerOf        ∷ w → Inner w
--   spaceToInner   ∷ w → DrawableSpace p (Depth w) → DrawableSpace p (Depth (Inner w))
--   styleToInner   ∷ w → StyleOf w → StyleOf (Inner w)

-- class Container d ⇒ WDrawable d where
--   assemble       ∷ (MonadIO m) ⇒ Settings PU → HC.ObjectStream → StyleOf d → Content d → m d
--   render         ∷ (MonadIO m) ⇒ d → m ()


-- * Styles
-- Widget composition is inherently parametrized.
-- Different kinds of composition are parametrized differently.
-- Different instances of composition compose different kinds of widgets.
-- Applicative much?

-- data In o i where
--   In ∷ --(Widget wo, Widget wi, StyleOf wo ~ o, StyleOf wi ~ i) ⇒ -- disabled by XXX/recursive pain
--     { insideOf ∷ o
--     , internal ∷ i
--     } → In o i
-- deriving instance (Show o, Show i) ⇒ Show (In o i)

-- data By o b where
--   By ∷ --(Widget wo, Widget wb, StyleOf wo ~ o, StyleOf wb ~ b) ⇒
--     { bOrigin  ∷ o
--     , bOrient  ∷ Orient Card
--     , bBeside  ∷ b
--     } → By o b
-- deriving instance (Show o, Show b) ⇒ Show (By o b)

-- data RRectS where
--   RRectS ∷
--     { rrCLBezel, rrCBorder, rrCDBezel, rrCBG ∷ Co Double
--     , rrThBezel, rrThBorder, rrThPadding ∷ Th Double
--     } → RRectS
-- deriving instance Show RRectS


-- * (): a null widget
-- instance Element () where
--   type  StyleOf () = ()
--   type  Content () = ()
--   type    Depth () = 0
-- instance Widget () where
--   query _settings        _style _content = pure End
--   make  _settings CW{..} _style _content        End = pure ()
--   draw            CW{..}                             _widget = pure ()


-- * New IR problems/questions:
--
-- - Cairo text size query is in IO
--    …'unsafePerformIO' is the answer, for now/until it breaks (query'text'size)
-- - where to feed Settings from
-- - where to feed text style from
-- - how to interpret screen size as a hint -- problematic as expected
-- - how to maintain style and content separation
-- - in effect, the entire chain is unclear:  (Settings, TextS, T.Text) → RProduct
-- - how do we remove Text's reliance on defaultWidth?:
--   - style (one-/multi-line?, content)
--   - in case of multi-line, there's ambiguity
-- - same as above, but more generally:
--   - downward constraint propagation needs to be informed by initial Direct Requirements
--     - those, that involve direct children only, not the entire subtrees
--     - formulated as ratios of parent, as a consequence
--
-- Design decisions (thanks to Dmitry Likhachev for a very helpful discussion):
--
--  1. We have to propagate constraints top-bottom, at least for informing minimum
--     requirements.
--  2. Two policies for overflow handling: minima shrinking and viewporting.
--  3. Lack of information is to be resolved by styling.
--  4. We need defaults for style, eg.:
--     - 1/nth of container, for constraints, playing downward minima propagation
--     - minima shrinking,
--  5. Initial constraints are soft, to inform minima
--  6. Initial constraints to be informed by a ratio-of-parent
--  7. Overall scheme:
--     - bottom-top, initial requirements: Style → Contents → (RAbsolute minimum, RParentRelative optimum)
--
-- Questions:
--
--  1. Should parents be able to inspect style of children?
--     - this can be useful at least for informing initial constraints
--     - probably extending/repurposing the HasRequires class
--

-- * Text

-- Problem statement:

--   1. Given:
--      - a tree of boxy styles
--      - with leaves:
--        - font
--        - content
--        - style
--   2. Build a tree of Flex Items

-- data TextStyle where
--   TextStyle ∷
--     { tsFontKey      ∷ FontKey
--     , tsMaxParaLines ∷ Int
--     , tsMaxLineChars ∷ Int
--     , tsColor        ∷ Co Double
--     } → TextS
-- deriving instance Show TextStyle

-- data Element where
--   Text ∷
--     { _query'size   ∷ Element → Di Double
--     , _style        ∷ Style
--     , _style'text   ∷ TextStyle
--     , _content'text ∷ Text
--     } → Element

-- text ∷ (AreaDict d) ⇒ FontKey → Int → Co Double → Ap (C d) a
-- text fk = lift .: TextS fk

-- text'canary ∷ (AreaDict d) ⇒ Ap (C d) a
-- text'canary = layout (LU $ po 0 0) (Cstr $ di 10 10) text

-- data Text where
--   Text ∷
--     { -- tPSpace       ∷ DrawableSpace True 1
--     -- ,
--       tStyle        ∷ StyleOf Text
--     , tFont         ∷ Font Bound PU
--     , tLayout       ∷ GIP.Layout
--     , tTextRef      ∷ IO.IORef T.Text
--     } → Text

-- -- | Sets the text content of WText, but doesn't update its rendering.
-- wtextSetText ∷ (MonadIO m) ⇒ Text → T.Text → m ()
-- wtextSetText Text{..} textVal = liftIO $ IO.writeIORef tTextRef textVal

-- instance Element Text where
--   type  StyleOf Text = TextS PU
--   type  Content Text = T.Text
--   type    Depth Text = 1
-- instance Widget Text where
  -- query Settings{..} TextS{..} initialText = do
  --   let Font{..} = lookupFont' fontmap tFontKey
  --   laySetMaxParaLines fDetachedLayout tMaxParaLines
  --   d ∷ Di (Unit PU) ← layRunTextForSize fDetachedLayout fDΠ defaultWidth initialText -- XXX/GHC/inference: weak
  --   pure $ mkSpace $ fromPU ∘ fromUnit fDΠ <$> d
  -- make Settings{..} (CW (Canvas Drawable{..} _ _ tFont@FontBinding{..} _))
  --      tStyle@(TextS _ _ _) tText tPSpace = do
  --   tLayout  ← makeTextLayout fbContext
  --   tTextRef ← liftIO $ IO.newIORef tText
  --   pure Text{..}
  -- draw (CW (Canvas (Drawable{..}) _ _ _ _))
  --      (Text (Sarea area@(Parea _ ltp@(Po lt)))
  --            TextS{..}
  --            (FontBinding Font{..} _) lay textRef) = do
  --   let Po rb = pareaSE area
  --       dim   = rb ^-^ lt
  --   laySetSize         lay fDΠ $ Di (PUs <$> dim)
  --   laySetMaxParaLines lay tMaxParaLines
  --   layDrawText dCairo dGIC lay ltp tColor =<< (liftIO $ IO.readIORef textRef)
  --   -- let V2 w h = ceiling <$> dim ∷ V2 Int
  --   -- layDrawText dGRC dGIC lay (po 0 0) (coOpaq 1 0 0) $
  --   --   T.pack $ printf "sz %d %d" w h

-- 
-- -- * Rounded rectangle
-- data RRect a where
--   RRect ∷
--     { rrPSpace ∷ DrawableSpace True (Depth (RRect a))
--     , rrStyle ∷ StyleOf (RRect a)
--     , rrInner ∷ a
--     } → RRect a
-- deriving instance (Show a, Show (StyleOf a)) ⇒ Show (RRect a)

-- instance Element a ⇒ Element (RRect a) where
--   type             StyleOf (RRect a) = In RRectS (StyleOf a) -- XXX/recursive pain
--   type             Content (RRect a) = Content a
--   type               Depth (RRect a) = 4 + Depth a
-- instance Widget a ⇒ Container (RRect a) where
--   type Inner (RRect a) = a
--   innerOf                 = rrInner
--   styleToInner _ (In _ s) = s
--   spaceToInner _ (Spc _ (Spc _ (Spc _ (Spc _ s)))) = s

-- instance Widget a ⇒ Widget (RRect a) where
--   query st@Settings{..} (In RRectS{..} inner) internals = do
--     innerSpace ← query st inner internals
--     pure $ (spaceGrow rrThBezel $ spaceGrow rrThBorder $ spaceGrow rrThBezel $ spaceGrow rrThPadding End)
--            <> innerSpace
--   make st@Settings{..} drawable rrStyle rrContent rrPSpace = do
--     let w = RRect{..} where rrInner = (⊥)    -- resolve circularity due to *ToInner..
--     make st drawable (styleToInner w rrStyle) rrContent (spaceToInner w rrPSpace) <&> (\x→ w { rrInner = x }) -- XXX/lens
--   draw canvas@(CW (Canvas (Drawable _ _ _ dCairo _ _ _ _ _) _ _ _ _))
--        (RRect (Spc obez (Spc bord (Spc ibez (Spc pad _))))
--               (In RRectS{..} _) inner) = do
--     runCairo dCairo $ do
--       let -- dCorn (RRCorn _ pos _ _) col = d pos col
--           ths@[oth, bth, ith, _]
--                         = fmap (Th ∘ _wiV ∘ wThL) [obez, bord, ibez, pad]
--           totpadx       = sum ths
--           or            =       R ∘ _thV $ (totpadx - oth/2)
--           br            = or - (R ∘ _thV $ (oth+bth)*0.6)
--           ir            = br - (R ∘ _thV $ (bth+ith)/2)
--       -- coSetSourceColor (co 0 1 0 1) >> GRC.paint
--       -- background & border arcs
--       let [n, ne, _, se, _, sw, _, nw] = wrapRoundedRectFeatures bord br bth
--       GRC.newPath >> thLineSet bth
--       forM_ [n, ne, se, sw, nw] $ executeFeature Nothing Nothing
--       coSetSourceColor rrCBG >>
--         GRC.fillPreserve
--       coSetSourceColor rrCBorder >>
--         GRC.stroke

--       thLineSet oth -- border bezels: light outer TL, dark outer SE
--       let [n, ne, e, se, s, sw, w, nw] = wrapRoundedRectFeatures obez or oth
--       GRC.newPath
--       (coSetSourceColor $ rrCLBezel) >>
--         (forM_ [w, nw, n] $ executeFeature Nothing Nothing) >> GRC.stroke
--       (coSetSourceColor $ rrCDBezel) >>
--         (forM_ [e, se, s] $ executeFeature Nothing Nothing) >> GRC.stroke
--       (coSetSourceColor $ rrCBorder) >>
--         GRC.newPath >> (executeFeature (Just rrCDBezel) (Just rrCLBezel) sw) >> GRC.stroke >>
--         GRC.newPath >> (executeFeature (Just rrCLBezel) (Just rrCDBezel) ne) >> GRC.stroke

--       thLineSet ith -- border bezels: dark inner TL, light inner SE
--       let [n, ne, e, se, s, sw, w, nw] = wrapRoundedRectFeatures ibez ir ith
--       GRC.newPath
--       (coSetSourceColor $ rrCDBezel) >>
--         (forM_ [w, nw, n] $ executeFeature Nothing Nothing) >> GRC.stroke
--       (coSetSourceColor $ rrCLBezel) >>
--         (forM_ [e, se, s] $ executeFeature Nothing Nothing) >> GRC.stroke
--       (coSetSourceColor $ rrCBorder) >>
--         GRC.newPath >> (executeFeature (Just rrCLBezel) (Just rrCDBezel) sw) >> GRC.stroke >>
--         GRC.newPath >> (executeFeature (Just rrCDBezel) (Just rrCLBezel) ne) >> GRC.stroke

--        ∷ GRCI.Render () -- XXX/GHC: an apparent type checker bug
--       -- ellipsized ← GIP.layoutIsEllipsized gip
--       -- (, ellipsized) <$> GIP.layoutGetPixelSize gip
--     draw canvas inner

-- 
-- -- * Canvas
-- --
-- -- Canvas is associated with a physical drawable surface.
-- data CanvasS (u ∷ UnitK) where
--   CanvasS ∷
--     { cFontKey      ∷ FontKey
--     } → CanvasS u
-- deriving instance Show (CanvasS u)

-- data Canvas a where
--   Canvas ∷
--     { cDrawable     ∷ Drawable
--     , cPSpace       ∷ DrawableSpace True (Depth a)
--     , cStyle        ∷ StyleOf (Canvas a)
--     , cFont         ∷ Font Bound PU
--     , cInner        ∷ a
--     } → Canvas a
-- data CanvasW where
--   CW ∷ Widget a ⇒ { cPoly ∷ Canvas a } → CanvasW

-- instance Widget a ⇒ Element (Canvas a) where
--   type             StyleOf (Canvas a) = In (CanvasS PU) (StyleOf a)
--   type             Content (Canvas a) = Content a
--   type             Depth   (Canvas a) = Depth a
-- instance Widget a ⇒ Container (Canvas a) where
--   type                  Inner (Canvas a) = a
--   innerOf                   = cInner
--   styleToInner   _ (In _ s) = s
--   spaceToInner   _       s  = s

-- instance Widget a ⇒ WDrawable (Canvas a) where
--   assemble settings@Settings{..} stream cStyle@(In (CanvasS cFontKey) innerStyle) innerContent = do
--     cPSpace   ← spacePin (po 0 0) <$> query settings innerStyle innerContent
--     cDrawable ← makeDrawable stream $ spaceDim cPSpace
--     cFont     ← bindFont (lookupFont' fontmap cFontKey) $ dGIC cDrawable
--     let w = Canvas{..} where cInner = (⊥)                -- resolve circularity due to *ToInner..
--     cInner ← make settings (CW w) innerStyle innerContent cPSpace
--     pure w { cInner = cInner }
--   render self@Canvas{..} = do
--     draw (CW self) cInner
--     drawableContentToGPU cDrawable

-- 
-- -- * Distributor
-- --
-- -- Distributor provides distribution (placement).
-- class Distributor a where


-- placeCanvas ∷ (MonadIO m, Widget a) ⇒ Canvas a → HC.Frame → Po Double → m ()
-- placeCanvas c f = framePutDrawable f (drawableOf c) ∘ (doubleToFloat <$>)
