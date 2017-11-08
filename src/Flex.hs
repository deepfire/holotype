{-# LANGUAGE GADTs, GeneralizedNewtypeDeriving, RankNTypes #-}
{-# LANGUAGE BangPatterns, MultiWayIf, OverloadedStrings, RecordWildCards, TemplateHaskell, TupleSections, UnicodeSyntax, ViewPatterns #-}
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
module Flex where

import           Control.Applicative
import           Control.Lens                      hiding (children)
import           Data.Maybe                               (fromMaybe)
import           Data.Monoid                              ((<>))
import qualified Data.Text.Lazy                    as TL
import           Linear                            hiding (trace)
import           Prelude.Unicode
import           Text.PrettyPrint.Leijen.Text      hiding ((<>), (<$>), space)

-- import           Debug.Trace                              (trace)
import           Text.Printf                              (printf)
import Elsewhere
import Flatland

data LRTB a where
  LRTB ∷
    { left           ∷ a
    , right          ∷ a
    , top            ∷ a
    , bottom         ∷ a
    } → LRTB a
    deriving (Show)
makeLenses ''LRTB

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

data Positioning where
  Relative           ∷ Positioning
  Absolute           ∷ Positioning
  deriving (Eq, Show)

data Direction where
  DirColumn          ∷ Direction
  DirColumnReverse   ∷ Direction
  DirRow             ∷ Direction
  DirRowReverse      ∷ Direction
  deriving (Eq, Show)

data Wrapping where
  NoWrap             ∷ Wrapping
  Wrap               ∷ Wrapping
  ReverseWrap        ∷ Wrapping
  deriving (Eq, Show)

data Item where
  Item ∷
    -- attributes: input
    { _it'size            ∷ Di   (Maybe Double)
    , _it'position        ∷ LRTB (Maybe Double)
    , _it'padding         ∷ LRTB Double
    , _it'margin          ∷ LRTB Double
    , _it'justify'content ∷ Alignment
    , _it'align'content   ∷ Alignment
    , _it'align'items     ∷ Alignment
    , _it'align'self      ∷ Alignment
    , _it'positioning     ∷ Positioning
    , _it'direction       ∷ Direction
    , _it'wrap            ∷ Wrapping
    , _it'grow            ∷ Int
    , _it'shrink          ∷ Int
    , _it'order           ∷ Maybe Int
    , _it'basis           ∷ Double
    -- frame: output
    , _it'po              ∷ Po Double
    , _it'di              ∷ Di Double
    -- parent
    , _it'children        ∷ [Item]
    -- should order children
    } → Item
    deriving (Show)
makeLenses ''Item

mkItem' ∷ Maybe Double → Maybe Double → [Item] → Item
mkItem' width height children =
  Item { _it'size            = Di $ V2 width height
       , _it'position        = LRTB Nothing Nothing Nothing Nothing
       , _it'padding         = LRTB 0 0 0 0
       , _it'margin          = LRTB 0 0 0 0
       , _it'justify'content = AlignStart
       , _it'align'content   = AlignStretch
       , _it'align'items     = AlignStart
       , _it'align'self      = AlignAuto
       , _it'positioning     = Relative
       , _it'direction       = DirColumn
       , _it'wrap            = NoWrap
       , _it'grow            = 0
       , _it'shrink          = 1
       , _it'order           = Nothing
       , _it'basis           = 0
       , _it'children        = children
       --
       , _it'po              = Po $ V2 0 0
       , _it'di              = Di $ V2 0 0
       }

mkItem ∷ Double → Double → [Item] → Item
mkItem width height = mkItem' (Just width) (Just height)

instance Monoid Item where
  mempty  = mkItem 0 0 []
  mappend = error "'⊕' for Item is not implemented."

child  ∷ Int → Traversal' Item Item
child n = it'children ∘ ix n

pretty'mdouble ∷ Maybe Double → Doc
pretty'mdouble Nothing  = char '*'
pretty'mdouble (Just x) = double x

pretty'mdi ∷ Di (Maybe Double) → Doc
pretty'mdi (Di (V2 ma mb)) = pretty'mdouble ma <:> pretty'mdouble mb

pretty'item ∷ Item → Doc
pretty'item Item{..} = text "Item size:" <> pretty'mdi _it'size <+>
                             text ("po:" <> ppV2 (_po'v _it'po)) <+>
                             text ("di:" <> ppV2 (_di'v _it'di))

instance Pretty Item where
  pretty = unreadable "" ∘ pretty'item


child'align ∷ Item → Item → Alignment
child'align child parent =
  if child^.it'align'self ≡ AlignAuto
  then parent^.it'align'items
  else child^.it'align'self

child'marginLT, child'marginRB ∷ Item → Vertical → Reverse → Double
child'marginLT Item{..} Vertical   Forward = left   _it'margin -- CHILD_MARGIN(child, left, top)
child'marginLT Item{..} Vertical   Reverse = top    _it'margin -- CHILD_MARGIN(child, top, left)
child'marginLT Item{..} Horisontal Forward = top    _it'margin -- CHILD_MARGIN(child, left, top)
child'marginLT Item{..} Horisontal Reverse = left   _it'margin -- CHILD_MARGIN(child, top, left)
child'marginRB Item{..} Vertical   Forward = right  _it'margin -- CHILD_MARGIN(child, right, bottom)
child'marginRB Item{..} Vertical   Reverse = bottom _it'margin -- CHILD_MARGIN(child, bottom, right)
child'marginRB Item{..} Horisontal Forward = bottom _it'margin -- CHILD_MARGIN(child, right, bottom)
child'marginRB Item{..} Horisontal Reverse = right  _it'margin -- CHILD_MARGIN(child, bottom, right)

child'size  ∷ Major → Lens' Item Double
child'size   (Major axis) = it'di ∘ di'd axis
child'size2 ∷ Minor → Lens' Item Double
child'size2  (Minor axis) = it'di ∘ di'd axis

child'pos   ∷ Major → Lens' Item Double
child'pos    (Major axis) = it'po ∘ po'd axis
child'pos2  ∷ Minor → Lens' Item Double
child'pos2   (Minor axis) = it'po ∘ po'd axis

data Vertical = Horisontal | Vertical    deriving (Eq, Show)
data Reverse  = Forward    | Reverse     deriving (Eq, Show)

data LayoutLine where
  LayoutLine ∷
    { _li'nchildren  ∷ Int
    , _li'size       ∷ Double
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
    -- int *ordered_indices -- ordering property ignored
    , _la'line'dim     ∷ Double    -- minor axis size
    , _la'flex'dim     ∷ Double    -- flexible part of the major axis size
    , _la'flex'grows   ∷ Int
    , _la'flex'shrinks ∷ Int
    , _la'pos2         ∷ Double    -- duplicate: "minor axis position"
    , _la'lines        ∷ [LayoutLine]
    , _la'lines'sizes  ∷ Double
    } → Layout
    deriving (Show)
makeLenses ''Layout

pretty'layout ∷ Layout → Doc
pretty'layout Layout{..} =
  let axes  = text $ showTL (fromMajor _la'major) <> ":" <> showTL (fromMinor _la'minor)
      wrap  = if _la'wrap then text " Wrap" else mempty
      dir   = text $ TL.take 4 $ showTL _la'vertical
      revMj = text $ (<>) "Maj" $ TL.take 4 $ showTL _la'reverse
      revMi = text $ (<>) "Min" $ TL.take 4 $ showTL _la'reverse2
  in text "Layout " <> wrap <+> axes <+> dir <+> revMj <+> revMi
     <+> text ("par:" <> ppV2 (V2 _la'size'dim _la'align'dim))
     <+> text ("chi:" <> ppV2 (V2 _la'flex'dim _la'line'dim))
     <+> text ("g/s:"  <> ppV2 (V2 _la'flex'grows _la'flex'shrinks))
     <+> text ("pos2:") <> double _la'pos2

instance Pretty Layout where
  pretty = unreadable "" ∘ pretty'layout


mkLayout ∷ Item → Di Double → Layout
mkLayout Item{..} (Di (V2 width' height')) =
  let width              = width'  - (left _it'padding + right  _it'padding)
      height             = height' - (top  _it'padding + bottom _it'padding)
      (,,,,,)
        _la'major
        _la'minor
        _la'vertical
        _la'reverse
        _la'size'dim
        _la'align'dim    = case _it'direction of
                             DirRow           → (Major X, Minor Y, Horisontal, Forward, width,  height)
                             DirRowReverse    → (Major X, Minor Y, Vertical,   Reverse, width,  height)
                             DirColumn        → (Major Y, Minor X, Vertical,   Forward, height, width)
                             DirColumnReverse → (Major Y, Minor X, Vertical,   Reverse, height, width)
      -- NB: child order property implementation has been skipped.
      (,,,)
        _la'flex'grows
        _la'flex'shrinks
        _la'flex'dim
        _la'line'dim
                         = (,,,) 0 0 0
                           (if _la'wrap then 0 else _la'align'dim) -- XXX: ⊥ in original code
      _la'wrap           = _it'wrap ≢ NoWrap
      reverse'wrapping   = _it'wrap ≡ ReverseWrap
      _la'reverse2       = if _la'wrap ∧ reverse'wrapping then Reverse else Forward
      _la'pos2           = case _la'wrap of
                             True  → if reverse'wrapping
                                     then _la'align'dim
                                     else 0                        -- XXX: ⊥ in original code
                             False → if _la'vertical ≡ Vertical
                                     then left _it'padding
                                     else top  _it'padding
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

is'vertical, is'reverse, is'reverse2 ∷ Layout → Bool
is'vertical = (≡Vertical) ∘ _la'vertical
is'reverse  = (≡Reverse)  ∘ _la'reverse
is'reverse2 = (≡Reverse)  ∘ _la'reverse2

count'relatives ∷ [Item] → Int
count'relatives = length ∘ filter ((≡Relative) ∘ (^.it'positioning))

layout_items ∷ Item → [Item] → Layout → (Layout, [Item])
-- Q: is this short-cut even needed?
-- layout_items _             []       l            = (,) l []
layout_items item@Item{..} children l@Layout{..} =
  -- Determine the major axis initial position and optional spacing.
  let (pos0, spacing0)  = (,) 0 0
      (pos1, spacing1)  = if _la'flex'grows ≡ 0 ∧
                             _la'flex'dim > 0
                          then let may'aligned = layout'align _it'justify'content _la'flex'dim (count'relatives children) False
                                   (pos', spacing') = flip fromMaybe may'aligned $ error "incorrect justify_content"
                               in (, spacing') $
                                  if is'reverse l
                                  then _la'size'dim - pos'
                                  else pos'
                          else (pos0, spacing0)
      pos2              = if is'reverse l
                          then pos1 - if is'vertical l then bottom _it'padding else right _it'padding
                          else pos1 + if is'vertical l then top    _it'padding else left  _it'padding
      -- This is suspicious: line 455 in flex.c
      l'                = if _la'wrap ∧ is'reverse2 l                 -- line 454: l→wrap implied by l→reverse2
                          then l & la'pos2 -~ (l^.la'line'dim)
                          else l
      layout'child ∷ Double → Item → (Double, Item)
      layout'child pos c@((^.it'positioning) → Absolute) = (,) pos c -- Already positioned.
      layout'child pos c0@Item{..} =
        -- Grow or shrink the major axis item size if needed.
        let flex'size   = if | l'^.la'flex'dim > 0 ∧ _it'grow   ≢ 0 → (l'^.la'flex'dim) ⋅ fromIntegral _it'grow   / fromIntegral (l'^.la'flex'grows)
                             | l'^.la'flex'dim < 0 ∧ _it'shrink ≢ 0 → (l'^.la'flex'dim) ⋅ fromIntegral _it'shrink / fromIntegral (l'^.la'flex'shrinks)
                             | otherwise → 0
            c1          = c0 &  child'size  _la'major +~ flex'size
            -- Set the minor axis position (and stretch the minor axis size if needed).
            align'size  = c1 ^. child'size2 _la'minor
            c'align     = child'align c1 item
            margin'LT   = child'marginLT c1 _la'vertical Forward
            margin'RB   = child'marginRB c1 _la'vertical Forward
            c2          = if c'align ≡ AlignStretch ∧ align'size ≡ 0
                          then c1 & child'size2 _la'minor .~ (l'^.la'line'dim - margin'LT - margin'RB)
                          else c1
            align'pos'δ = if | c'align ≡ AlignEnd     →              l'^.la'line'dim - align'size      - margin'RB
                             | c'align ≡ AlignCenter  → margin'LT + (l'^.la'line'dim - align'size) / 2 - margin'RB
                             | c'align ≡ AlignStretch ∨
                               c'align ≡ AlignStart   → margin'LT
                             | otherwise → error "invalid align_self"
            align'pos   = l'^.la'pos2 + align'pos'δ
            c3          = c2 & child'pos2 _la'minor .~ align'pos
            -- Set the main axis position.
            margin'LTr  = child'marginLT c3 _la'vertical Reverse
            margin'RBr  = child'marginRB c3 _la'vertical Reverse
            pos'        = if _la'reverse ≡ Reverse
                          then pos - margin'RBr - c3^.child'size _la'major
                          else pos + margin'LTr
            c4          = c3 & child'pos _la'major .~ pos'
            pos''       = if _la'reverse ≡ Reverse
                          then pos' - spacing1 - margin'LTr
                          else pos' + c4^.child'size _la'major + spacing1 + margin'RBr
        in (,) pos'' $ layout_item c4 (c4^.it'di)
      step ∷ [Item] → Double → [Item] → [Item]
      step []     _   acc = reverse acc
      step (c:cs) pos acc = let (pos', c') = layout'child pos c
                            in step cs pos' (c':acc)
      children'         = step children pos2 []
      l''               = if not _la'wrap ∨ is'reverse2 l' then l'
                          else l' & la'pos2 +~ (l'^.la'line'dim)
      l'''              = if not _la'wrap ∨ _it'align'content ≡ AlignStart then l''
                          else l'' & la'lines %~ (LayoutLine (length children') (l''^.la'line'dim) :)
                                   & la'lines'sizes +~ l''^.la'line'dim
  in (,) l''' children'

layout_item ∷ Item → Di Double → Item
layout_item item@(_it'children → []) _ = item
layout_item item cstr =
  let layout = layout'reset $ mkLayout item cstr
      lay'one ∷ Layout → Item → ([Item], [Item]) → (Layout, Item, ([Item], [Item]))
      lay'one l@Layout{..} c@(_it'positioning → Absolute) (to'process, processed) =
        let abs'size (Just val)  _           _          _   = val
            abs'size  Nothing   (Just pos1) (Just pos2) dim = dim - pos2 - pos1
            abs'size  _          _           _          _   = 0
            abs'pos  (Just pos1) _           _          _   = pos1
            abs'pos   Nothing   (Just pos2) (Just size) dim = dim - size - pos2
            abs'pos   _          _           _          _   = 0
            (pos, dim) = (_it'position c, _it'size c)
            c'dim      = Di $ V2 (abs'size (dim^.di'd X) (left pos) (right  pos) (cstr^.di'd X))
                                 (abs'size (dim^.di'd Y) (top  pos) (bottom pos) (cstr^.di'd Y))
            c'pos      = Po $ V2 (abs'pos  (left pos) (right  pos) (dim^.di'd X) (cstr^.di'd X))
                                 (abs'pos  (top  pos) (bottom pos) (dim^.di'd Y) (cstr^.di'd Y))
            c'         = c & it'di .~ c'dim
                           & it'po .~ c'pos
            c''         = layout_item c' c'dim
        in (,,) l c'' (c'':to'process, processed)
      lay'one l@Layout{..} c (to'process, processed) =
        let c' = c & child'size  _la'major .~ (fromMaybe 0 $ partial (>0) (c^.it'basis) <|>
                                               (c^.it'size.di'd (fromMajor _la'major)))
                   & child'size2 _la'minor .~ flip fromMaybe (c^.it'size.di'd (fromMinor _la'minor))
                                              (cstr^.di'd (if _la'vertical ≡ Vertical then X else Y) -
                                                     child'marginLT c _la'vertical Forward -
                                                     child'marginRB c _la'vertical Forward)
            -- NB: self_sizing callback implementation ignored
            no'wrap = not (layout^.la'wrap)
            c'size  = c'^.child'size _la'major
            (,,) l' tp' p' = let (l',tp',p') = if no'wrap ∨ l^.la'flex'dim ≥ c'size then (l, to'process, processed)
                                               else let (,) l' pcs = layout_items item (reverse to'process) l
                                                    in (,,) (layout'reset l') [] (processed <> pcs)
                                 c'size2     = c'^.child'size2 _la'minor
                             in (,tp',p') $ if no'wrap ∨ c'size2 ≤ l'^.la'line'dim then l'
                                            else l' & la'line'dim .~ c'size2
            l'' = l' & la'flex'grows   +~ c'^.it'grow
                     & la'flex'shrinks +~ c'^.it'shrink
                     & la'flex'dim     -~ c'size
                       + child'marginLT c' _la'vertical Reverse
                       + child'marginRB c' _la'vertical Reverse
        in (,,) l'' c' (c':tp', p')
      step ∷ [Item] → Layout → ([Item], [Item]) → (Layout, ([Item], [Item]))
      step []     l (to'process, processed) = (l, (to'process, processed))
      step (x:xs) l (to'process, processed) =
        let (,,) l' x' (tp', p') = lay'one l x (to'process, processed)
        in step xs l' (tp', p')
      (layout'@Layout{..},
       (to'process, processed)) = step (item^.it'children) layout ([], [])
      (,) layout'' processed' = layout_items item (reverse to'process) layout'
      layout'lines = reverse $ layout''^.la'lines
      children'  = processed <> processed'
      -- In wrap mode if the 'align_content' property changed from its default
      -- value, we need to tweak the position of each line accordingly.
      children'' = if not _la'wrap ∨ (item^.it'align'content) ≡ AlignStart ∨ length layout'lines ≡ 0 then children'
                   else let (,) pos  spacing  = (0 ∷ Double, 0 ∷ Double)
                            flex'dim          = layout''^.la'align'dim - (layout''^.la'lines'sizes)
                            may'aligned       = layout'align (item^.it'align'content) flex'dim
                                                (length $ layout'lines) True
                            (,) pos' spacing' = if flex'dim ≤ 0 then (,) pos spacing
                                                else flip fromMaybe may'aligned $ error "incorrect align_content"
                            (,) pos'' old'pos = if _la'reverse2 ≢ Reverse then (,) pos' 0
                                                else (,) (layout''^.la'align'dim - pos') (layout''^.la'align'dim)
                            line'step ∷ [LayoutLine] → [Item] → [[Item]] → Double → Double → ([Item], Double, Double)
                            line'step [] []     acc pos old'pos = (,,) (concat $ reverse acc) pos old'pos
                            line'step [] (u:us) _   _   _       = error $ printf "Invariant failed: %d children left unaccounted for." (length us + 1)
                            line'step (LayoutLine{..}:ls) cs acc pos old'pos =
                              let (,) pos' old'pos'   = if _la'reverse2 ≢ Reverse then (,) pos  old'pos
                                                        else (,) (pos - _li'size - spacing') (old'pos - _li'size)
                                  (,) line rest       = splitAt _li'nchildren cs
                                  -- Re-position the children of this line, honoring any child alignment previously set within the line
                                  line'               = line <&> \c→ if c^.it'positioning ≡ Absolute then c
                                                                     else c & child'pos2 _la'minor +~ pos' - old'pos'
                                  (,) pos'' old'pos'' = if _la'reverse2 ≡ Reverse then (,) pos' old'pos'
                                                        else (,) (pos + _li'size + spacing') (old'pos + _li'size)
                              in line'step ls rest (line':acc) pos'' old'pos''
                        in (^._1) $ line'step layout'lines children' [] pos'' old'pos
  in item & it'children .~ children''

flex_layout ∷ Item → Item
flex_layout x = layout_item x $ fromMaybe (error "Root missing coord.") <$> x^.it'size


-- * Test accessories.
--
type Frame a = (Po Double, Di Double)

frame  ∷ Lens' Item (Po Double, Di Double)
frame f it@Item{..} = (\(p,d)→ it { _it'po = p, _it'di = d }) <$> f (_it'po, _it'di)

frame' ∷ Po Double → Di Double → Frame a
frame' = (,)

frame'test ∷ (Frame a → Bool) → Item → Bool
frame'test f = f ∘ (^.frame)

frame'of'is ∷ Po Double → Di Double → Item → Bool
frame'of'is p d = ((p, d)≡) ∘ (^.frame)
