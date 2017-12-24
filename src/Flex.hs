{-# LANGUAGE DeriveFunctor, DeriveFoldable, DeriveTraversable, GeneralizedNewtypeDeriving, StandaloneDeriving #-}      -- Deriving
{-# LANGUAGE FlexibleInstances, GADTs, InstanceSigs, RankNTypes, ScopedTypeVariables #-}          -- Types
{-# LANGUAGE BangPatterns, MultiWayIf, OverloadedStrings, RecordWildCards #-}       -- Syntactic
{-# LANGUAGE TemplateHaskell, TupleSections, UnicodeSyntax, ViewPatterns #-}
{-# OPTIONS_GHC -Wextra #-}
-- Development-only muffles.
{-# OPTIONS_GHC -Wno-unused-binds -Wno-unused-matches #-}
--
-- Flex:  a flexbox layout implementation, based on github.com/xamarin/flex.
--
-- Two caveats:
--
-- 1. Order property and size callbacks are not implemented.
-- 2. Initially, this is a very direct translation from C -- and it shows a lot.
--
module Flex
  ( -- * Language of Style
    LRTB(..), left, right, top, bottom
  , Alignment(..)
  , Direction(..)
  , Positioning(..)
  , Wrapping(..)
  --
  , Style(..), defaultStyle
  , padding
  , margin
  , justify'content
  , align'content
  , align'items
  , align'self
  , positioning
  , direction
  , wrap
  , grow
  , shrink
  , order
  , basis
  -- * Item
  , Item(..), mkItem
  --
  , Place(..)
  , size
  , absolute
  -- * Item
  , Item(..), mkItem, _mkItem, _mkItem'
  , style
  , place
  , area
  , child, children
  , this
  -- * Layout API
  , layout
  )
where

import           HoloPrelude

import qualified Data.Text.Lazy                    as TL
import           Linear                            hiding (basis, trace)
import           Prelude                           hiding (floor)
import qualified Text.PrettyPrint.Leijen.Text      as WL

import           Elsewhere
import           Flatland


-- * A language of Style
--
data LRTB a where
  LRTB ∷
    { _left          ∷ a
    , _right         ∷ a
    , _top           ∷ a
    , _bottom        ∷ a
    } → LRTB a
    deriving (Functor, Show)
makeLenses ''LRTB

instance Applicative LRTB where
  pure x = LRTB x x x x
  LRTB f0 f1 f2 f3 <*> LRTB v0 v1 v2 v3 =
    LRTB (f0 v0) (f1 v1) (f2 v2) (f3 v3)

data Alignment where
  AlignAuto          ∷ Alignment
  AlignStretch       ∷ Alignment
  AlignCenter        ∷ Alignment
  AlignStart         ∷ Alignment
  AlignEnd           ∷ Alignment
  AlignSpaceBetween  ∷ Alignment
  AlignSpaceAround   ∷ Alignment
  AlignSpaceEvenly   ∷ Alignment
  deriving (Eq, Show)

data Direction where
  DirColumn          ∷ Direction
  DirColumnReverse   ∷ Direction
  DirRow             ∷ Direction
  DirRowReverse      ∷ Direction
  deriving (Eq, Show)

data Positioning where
  Relative           ∷ Positioning
  Absolute           ∷ Positioning
  deriving (Eq, Show)

data Wrapping where
  NoWrap             ∷ Wrapping
  Wrap               ∷ Wrapping
  ReverseWrap        ∷ Wrapping
  deriving (Eq, Show)

data Style where
  Style ∷
    { _padding         ∷ LRTB Double
    , _margin          ∷ LRTB Double
    , _justify'content ∷ Alignment
    , _align'content   ∷ Alignment
    , _align'items     ∷ Alignment
    , _align'self      ∷ Alignment
    , _positioning     ∷ Positioning
    , _direction       ∷ Direction
    , _wrap            ∷ Wrapping
    , _grow            ∷ Int
    , _shrink          ∷ Int
    , _order           ∷ Maybe Int
    , _basis           ∷ Double
    } → Style
    deriving (Show)
makeLenses ''Style

defaultStyle ∷ Style
defaultStyle = Style
  { _padding         = LRTB 0 0 0 0
  , _margin          = LRTB 0 0 0 0
  , _justify'content = AlignStart
  , _align'content   = AlignStretch
  , _align'items     = AlignStart
  , _align'self      = AlignAuto
  , _positioning     = Relative
  , _direction       = DirColumn
  , _wrap            = NoWrap
  , _grow            = 0
  , _shrink          = 1
  , _order           = Nothing
  , _basis           = 0
  }

instance Monoid Style where
  mempty  = defaultStyle
  mappend = error "Monoidal append not implemented for Flex.Style."

data Place where
  Place ∷
    { _size          ∷ Di   (Maybe Double)
    , _absolute      ∷ LRTB (Maybe Double)
    } → Place
    deriving (Show)
makeLenses ''Place

instance Monoid Place where
  mempty
    = Place { _size     = (Di $ V2 Nothing Nothing)
            , _absolute = LRTB Nothing Nothing Nothing Nothing
            }
  Place ls la `mappend` Place rs ra
    = Place (liftA2 (<|>) ls rs) (liftA2 (<|>) la ra)


-- * The Item
--
data Item a where
  Item ∷
    { _style        ∷ Style
    , _place        ∷ Place
    , _area         ∷ Area Double
    , _children     ∷ [Item a]
    , _this         ∷ a
    } → Item a
    deriving (Functor, Foldable, Traversable)
deriving instance Show a ⇒ Show (Item a)
makeLenses ''Item

child  ∷ Int → Traversal' (Item a) (Item a)
child n = children ∘ ix n

mkItem ∷ a → [Item a] → Item a
mkItem _item _children =
  let _place    = mempty
      _style    = defaultStyle
      _area     = Area (po 0 0) (di 0 0)
  in Item{..}

_mkItem' ∷ Maybe Double → Maybe Double → a → [Item a] → Item a
_mkItem' width height _item _children =
  mkItem _item _children & place.size .~ (Di $ V2 width height)

_mkItem ∷ Double → Double → a → [Item a] → Item a
_mkItem width height = _mkItem' (Just width) (Just height)


-- * Instances
--
instance Pretty a ⇒ Pretty (Item a) where
  pretty = unreadable "" ∘ pretty'item

--
-- * no user-serviceable parts below, except for the 'layout' function at the very bottom.
--


pretty'mdouble ∷ Maybe Double → Doc
pretty'mdouble Nothing  = WL.char '*'
pretty'mdouble (Just x) = WL.double x

pretty'mdi ∷ Di (Maybe Double) → Doc
pretty'mdi (Di (V2 ma mb)) = pretty'mdouble ma <:> pretty'mdouble mb

pretty'item ∷ Item a → Doc
pretty'item Item{..} = WL.text "Item size:"
  <>  pretty'mdi (_size _place)
  <+> pretty'Area _area


item'marginLT, item'marginRB ∷ Item a → Vertical → Reverse → Double
item'marginLT Item{..} Vertical   Forward = _style & _margin & _left
item'marginLT Item{..} Vertical   Reverse = _style & _margin & _top
item'marginLT Item{..} Horisontal Forward = _style & _margin & _top
item'marginLT Item{..} Horisontal Reverse = _style & _margin & _left
item'marginRB Item{..} Vertical   Forward = _style & _margin & _right
item'marginRB Item{..} Vertical   Reverse = _style & _margin & _bottom
item'marginRB Item{..} Horisontal Forward = _style & _margin & _bottom
item'marginRB Item{..} Horisontal Reverse = _style & _margin & _right

item'size  ∷ Major → Lens' (Item a) Double
item'size   (Major axis) = area ∘ area'b ∘ di'd axis
item'size2 ∷ Minor → Lens' (Item a) Double
item'size2  (Minor axis) = area ∘ area'b ∘ di'd axis

item'pos   ∷ Major → Lens' (Item a) Double
item'pos    (Major axis) = area ∘ area'a ∘ po'd axis
item'pos2  ∷ Minor → Lens' (Item a) Double
item'pos2   (Minor axis) = area ∘ area'a ∘ po'd axis


data Vertical = Horisontal | Vertical    deriving (Eq, Show)
data Reverse  = Forward    | Reverse     deriving (Eq, Show)

data LayoutLine where
  LayoutLine ∷
    { _li'nchildren ∷ Int
    , _li'size      ∷ Double
    } → LayoutLine
    deriving (Show)
makeLenses ''LayoutLine

data Layout where
  Layout ∷
    { _la'wrap         ∷ Bool
    , _la'reverse      ∷ Reverse
    , _la'reverse2     ∷ Reverse
    , _la'vertical     ∷ Vertical
    , _la'major        ∷ Major
    , _la'minor        ∷ Minor
    , _la'size'dim     ∷ Double    -- major axis parent size
    , _la'align'dim    ∷ Double    -- minor axis parent size
    , _la'line'dim     ∷ Double    -- minor axis size
    , _la'flex'dim     ∷ Double    -- flexible part of the major axis size
    , _la'flex'grows   ∷ Int
    , _la'flex'shrinks ∷ Int
    , _la'pos2         ∷ Double    -- minor axis position
    , _la'lines        ∷ [LayoutLine]
    , _la'lines'sizes  ∷ Double
    } → Layout
    deriving (Show)
makeLenses ''Layout

pretty'layout ∷ Layout → Doc
pretty'layout Layout{..} =
  let axes  = WL.text $ showTL (fromMajor _la'major) <> ":" <> showTL (fromMinor _la'minor)
      wrap  = if _la'wrap then WL.text " Wrap" else mempty
      dir   = WL.text $ TL.take 4 $ showTL _la'vertical
      revMj = WL.text $ (<>) "Maj" $ TL.take 4 $ showTL _la'reverse
      revMi = WL.text $ (<>) "Min" $ TL.take 4 $ showTL _la'reverse2
  in WL.text "Layout " <> wrap <+> axes <+> dir <+> revMj <+> revMi
     <+> WL.text ("par:" <> ppV2 (V2 _la'size'dim _la'align'dim))
     <+> WL.text ("chi:" <> ppV2 (V2 _la'flex'dim _la'line'dim))
     <+> WL.text ("g/s:"  <> ppV2 (V2 _la'flex'grows _la'flex'shrinks))
     <+> WL.text ("pos2:") <> WL.double _la'pos2

instance Pretty Layout where
  pretty = unreadable "" ∘ pretty'layout


mkLayout ∷ Item a → Layout
mkLayout Item{..} =
  let V2 width' height'  = _area^.area'b.di'v
      Style{..}          = _style
      width              = width'  - (_padding^.left + _padding^.right)
      height             = height' - (_padding^.top  + _padding^.bottom)
      (,,,,,)
        _la'major
        _la'minor
        _la'vertical
        _la'reverse
        _la'size'dim
        _la'align'dim    = case _direction of
                             DirRow           → (Major X, Minor Y, Horisontal, Forward, width,  height)
                             DirRowReverse    → (Major X, Minor Y, Vertical,   Reverse, width,  height)
                             DirColumn        → (Major Y, Minor X, Vertical,   Forward, height, width)
                             DirColumnReverse → (Major Y, Minor X, Vertical,   Reverse, height, width)
      -- NB: child order property implementation has been skipped.
      (,,,)
        _la'flex'grows
        _la'flex'shrinks
        _la'flex'dim
        _la'line'dim     = (,,,) 0 0 0
                           (if _la'wrap then 0 else _la'align'dim) -- XXX: ⊥ in original code
      _la'wrap           = _wrap ≢ NoWrap
      reverse'wrapping   = _wrap ≡ ReverseWrap
      _la'reverse2       = if _la'wrap ∧ reverse'wrapping then Reverse else Forward
      _la'pos2           = case _la'wrap of
                             True  → if reverse'wrapping
                                     then _la'align'dim
                                     else 0                        -- XXX: ⊥ in original code
                             False → if _la'vertical ≡ Vertical
                                     then _padding^.left
                                     else _padding^.top
      _la'lines          = []
      _la'lines'sizes    = 0
  in Layout{..}

layout'reset ∷ Layout → Layout
layout'reset l@Layout{..} = l
  & la'line'dim     .~ (if _la'wrap then 0 else _la'align'dim)
  & la'flex'dim     .~ _la'size'dim
  & la'flex'grows   .~ 0
  & la'flex'shrinks .~ 0

-- | Yield (Position, Spacing)
layout'align  ∷ Alignment → Double → Int → Bool → Maybe (Double, Double)
layout'align _ 0        _ _ = Nothing
layout'align a flex'dim c d = layout'align' a flex'dim c d

layout'align' ∷ Alignment → Double → Int → Bool → Maybe (Double, Double)
layout'align' align flex'dim nchilds stretch'allowed
  | AlignStart        ← align = Just $ (,) 0 0
  | AlignEnd          ← align = Just $ (,) flex'dim 0
  | AlignCenter       ← align = Just $ (,) (flex'dim / 2) 0
  | AlignSpaceBetween ← align = Just $ (0,) $ if (nchilds ≡ 0) then 0
                                              else (flex'dim / fromIntegral (nchilds - 1))
  | AlignSpaceAround  ← align = Just $ if (nchilds ≡ 0) then (0, 0)
                                       else let spacing = flex'dim / fromIntegral nchilds
                                            in (spacing / 2, spacing)
  | AlignSpaceEvenly  ← align = Just $ if (nchilds ≡ 0) then (0, 0)
                                       else let spacing = flex'dim / fromIntegral (nchilds + 1)
                                            in (spacing,     spacing)
  | AlignStretch      ← align = if not stretch'allowed then Nothing
                                else Just (0, flex'dim / fromIntegral nchilds)
  | AlignAuto         ← align = Nothing

count'relatives ∷ [Item a] → Int
count'relatives = length ∘ filter ((≡Relative) ∘ (^.style.positioning))

layout_items ∷ Item a → [Item a] → Layout → (Layout, [Item a])
layout_items _             []       l            = (,) l []
layout_items item@Item{..} children l@Layout{..} =
  -- Determine the major axis initial position and optional spacing.
  let Style{..}         = _style
      (pos1, spacing1)  = if _la'flex'grows ≡ 0 ∧
                             _la'flex'dim > 0
                          then let may'aligned = layout'align _justify'content _la'flex'dim (count'relatives children) False
                                   (pos', spacing') = flip fromMaybe may'aligned $ error "incorrect justify_content"
                               in (, spacing') $
                                  if _la'reverse ≡ Reverse
                                  then _la'size'dim - pos'
                                  else pos'
                          else (,) 0 0
      pos2              = if _la'reverse ≡ Reverse
                          then pos1 - if _la'vertical ≡ Vertical then _padding^.bottom else _padding^.right
                          else pos1 + if _la'vertical ≡ Vertical then _padding^.top    else _padding^.left
      -- This is suspicious: line 455 in flex.c
      l'                = if _la'wrap ∧ _la'reverse2 ≡ Reverse       -- line 454: l→wrap implied by l→reverse2
                          then l & la'pos2 -~ _la'line'dim
                          else l
      layout'children ∷ Double → [Item a] → [Item a] → (Double, [Item a])
      layout'children pos  []                                      acc = (,) pos (reverse acc)
      layout'children pos (c@((^.style.positioning) → Absolute):rest) acc = layout'children pos rest (c:acc) -- Already positioned.
      layout'children pos (c@Item{..}:rest) acc =
        -- Grow or shrink the major axis item size if needed.
        let flex'size   = if | l'^.la'flex'dim > 0 ∧ _style^.grow   ≢ 0 → (l'^.la'flex'dim) ⋅ fromIntegral (_style^.grow)   / fromIntegral (l'^.la'flex'grows)
                             | l'^.la'flex'dim < 0 ∧ _style^.shrink ≢ 0 → (l'^.la'flex'dim) ⋅ fromIntegral (_style^.shrink) / fromIntegral (l'^.la'flex'shrinks)
                             | otherwise → 0
            c1          = c &  item'size  _la'major +~ flex'size
            -- Set the minor axis position (and stretch the minor axis size if needed).
            align'size  = c1 ^. item'size2 _la'minor
            c'align     = if c1^.style.align'self ≡ AlignAuto
                          then item^.style.align'items
                          else c1^.style.align'self
            margin'LT   = item'marginLT c1 _la'vertical Forward
            margin'RB   = item'marginRB c1 _la'vertical Forward
            c2          = if c'align ≡ AlignStretch ∧ align'size ≡ 0
                          then c1 & item'size2 _la'minor .~ (l'^.la'line'dim - margin'LT - margin'RB)
                          else c1
            align'pos'δ = if | c'align ≡ AlignEnd     →              l'^.la'line'dim - align'size      - margin'RB
                             | c'align ≡ AlignCenter  → margin'LT + (l'^.la'line'dim - align'size) / 2 - margin'RB
                             | c'align ≡ AlignStretch ∨
                               c'align ≡ AlignStart   → margin'LT
                             | otherwise → error "invalid align_self"
            align'pos   = l'^.la'pos2 + align'pos'δ
            c3          = c2 & item'pos2 _la'minor .~ align'pos
            -- Set the main axis position.
            margin'LTr  = item'marginLT c3 _la'vertical Reverse
            margin'RBr  = item'marginRB c3 _la'vertical Reverse
            pos'        = if _la'reverse ≡ Reverse
                          then pos - margin'RBr - c3^.item'size _la'major
                          else pos + margin'LTr
            c4          = c3 & item'pos _la'major .~ pos'
            pos''       = if _la'reverse ≡ Reverse
                          then pos' - spacing1 - margin'LTr
                          else pos' + c4^.item'size _la'major + spacing1 + margin'RBr
        in layout'children pos'' rest $ (:acc) $ layout_item c4
      children'         = snd $ layout'children pos2 children []
      l''               = if not _la'wrap ∨ _la'reverse2 ≡ Reverse then l'
                          else l' & la'pos2 +~ (l'^.la'line'dim)
      l'''              = if not _la'wrap ∨ _align'content ≡ AlignStart then l''
                          else l'' & la'lines %~ (LayoutLine (length children') (l''^.la'line'dim) :)
                                   & la'lines'sizes +~ l''^.la'line'dim
  in (,) l''' children'

layout_item ∷ ∀ a. Item a → Item a
layout_item p@(_children → []) = p
layout_item p@Item{..} =
  let cstr   = p^.area.area'b
      assign'sizes ∷ [Item a] → Layout → [Item a] → [Item a] → (Layout, [Item a], [Item a])
      assign'sizes []                                       l            sized positioned = (,,) l (reverse sized) positioned
      assign'sizes (c@((^.style.positioning) → Absolute):rest) l@Layout{..} sized positioned =
        let abs'size (Just val)  _           _          _   = val
            abs'size  Nothing   (Just pos1) (Just pos2) dim = dim - pos2 - pos1
            abs'size  _          _           _          _   = 0
            abs'pos  (Just pos1) _           _          _   = pos1
            abs'pos   Nothing   (Just pos2) (Just size) dim = dim - size - pos2
            abs'pos   _          _           _          _   = 0
            (pos, dim) = (c^.place.absolute, c^.place.size)
            c' = c & area .~ Area (Po $ V2 (abs'pos  (pos^.left) (pos^.right) (dim^.di'd X) (cstr^.di'd X))
                                           (abs'pos  (pos^.top)  (pos^.bottom) (dim^.di'd Y) (cstr^.di'd Y)))
                                  (Di $ V2 (abs'size (dim^.di'd X) (pos^.left) (pos^.right) (cstr^.di'd X))
                                           (abs'size (dim^.di'd Y) (pos^.top) (pos^.bottom) (cstr^.di'd Y)))
                           & layout_item
        in assign'sizes rest l (c':sized) positioned
      assign'sizes (c:rest) l@Layout{..} sized positioned =
        let c'size  = fromMaybe 0 $ partial (>0) (c^.style.basis) <|>
                                                 (c^.place.size.di'd (fromMajor _la'major))
            c'size2 = flip fromMaybe (c^.place.size.di'd (fromMinor _la'minor))
                                     (cstr^.di'd (if _la'vertical ≡ Vertical then X else Y) -
                                      item'marginLT c _la'vertical Forward -
                                      item'marginRB c _la'vertical Forward)
            -- NB: self_sizing callback implementation ignored
            (,,) sized' positioned' l'
              = if not _la'wrap ∨ l^.la'flex'dim ≥ c'size then (,,) sized positioned l
                else let (,) lay' positioned' = layout_items p (reverse sized) l
                     in (,,) [] (positioned <> positioned') (layout'reset lay')
            l'' = l' & la'line'dim %~ (\line'dim→ if not _la'wrap ∨ c'size2 ≤ line'dim then line'dim
                                                  else c'size2)
                     & la'flex'grows   +~ c'^.style.grow
                     & la'flex'shrinks +~ c'^.style.shrink
                     & la'flex'dim     -~ c'size + item'marginLT c' _la'vertical Reverse
                                                 + item'marginRB c' _la'vertical Reverse
            c' = c & item'size  _la'major .~ c'size
                   & item'size2 _la'minor .~ c'size2
        in assign'sizes rest l'' (c':sized') positioned'
      -- 1. Assign sizes (and some of positions, if wrapping):
      (,,) lay       sized positioned  = assign'sizes _children (layout'reset $ mkLayout p) [] []
      -- 2. Assign remainder of positions:
      (,)  lay'@Layout{..} positioned' = layout_items p sized lay
      -- 3. Merge wrap-generated positionals & the results of remainder processing:
      children'  = positioned <> positioned'
      -- 4. In wrap mode if the 'align_content' property changed from its default
      --    value, we need to tweak the position of each line accordingly.
      align'children ∷ Layout → [Item a] → [Item a]
      align'children Layout{..} cs =
        let flex'dim          = _la'align'dim - _la'lines'sizes
            may'aligned       = layout'align (_style^.align'content) flex'dim (length _la'lines) True
            (,) pos' spacing' = if flex'dim ≤ 0 then (,) 0 0
                                else flip fromMaybe may'aligned $ error "incorrect align_content"
            (,) pos'' old'pos = if _la'reverse2 ≢ Reverse then (,) pos' 0
                                else (,) (_la'align'dim - pos') _la'align'dim
            line'step ∷ [LayoutLine] → [Item a] → [[Item a]] → Double → Double → ([Item a], Double, Double)
            line'step [] []     acc pos old'pos = (,,) (concat $ reverse acc) pos old'pos
            line'step [] (u:us) _   _   _       = error $ printf "Invariant failed: %d children left unaccounted for." (length us + 1)
            line'step (LayoutLine{..}:ls) cs acc pos old'pos =
              let (,) pos' old'pos'   = if _la'reverse2 ≢ Reverse then (,) pos  old'pos
                                        else (,) (pos - _li'size - spacing') (old'pos - _li'size)
                  (,) line rest       = splitAt _li'nchildren cs
                  -- Re-position the children of this line, honoring any child alignment previously set within the line
                  line'               = line <&> \c→ if c^.style.positioning ≡ Absolute then c
                                                     else c & item'pos2 _la'minor +~ pos' - old'pos'
                  (,) pos'' old'pos'' = if _la'reverse2 ≡ Reverse then (,) pos' old'pos'
                                        else (,) (pos + _li'size + spacing') (old'pos + _li'size)
              in line'step ls rest (line':acc) pos'' old'pos''
        in (^._1) $ line'step (reverse _la'lines) cs [] pos'' old'pos
  in p & children .~ if _la'wrap ∧ _style^.align'content ≢ AlignStart
                     then align'children lay' children'
                     else children'


-- | Lay out children according to their and item's properties.
--   Size is taken from the 'item', origin is fixed to 0:0.
layout ∷ Item a → Item a
layout x = layout_item $
  x & area.area'b .~ (fromMaybe (error "Root missing coord.") <$> x^.place.size)


-- * Playground
--
type Ty = (Style, Di Double)

root ∷ Item ()
root = layout $
  _mkItem 100 100 () cs

cs ∷ [Item a]
cs = []


-- `π` ≡ preimage
-- π ∷ 
-- π = (,) (defaultStyle & )
--     $ di 10 10

-- proof ∷ Item 
