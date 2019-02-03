{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wextra #-}
-- Development-only muffles.
{-# OPTIONS_GHC -Wno-unused-binds -Wno-unused-matches #-}
--
-- Flex:  a flexbox layout implementation, based on github.com/xamarin/flex,
--        and https://www.w3.org/TR/css-flexbox-1/
--
-- Two caveats:
--
-- 1. Order property and size callbacks are not implemented.
-- 2. Initially, this is a very direct translation from C -- and it shows a lot.
--
module Graphics.Flex
  ( -- * Language of Geo
    LRTB(..), left, right, top, bottom
  , Alignment(..)
  , Direction(..)
  , Positioning(..)
  , Wrapping(..)
  , child
  , walk
  , dump, ppItemArea, showItemArea, ppItemSize
  --
  , Geo(..), defGeo, defGeoDiff, ppdefGeoDiff
  , padding
  , margin
  , absolute
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
  --
  -- * Flex class
  , Flex(..)
  -- * Layout API
  , layout
  )
where
import qualified Data.Text                             as T
import           ExternalImports

import           Graphics.Flatland


-- * A language of Geo
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

data Geo where
  Geo ∷
    { _padding         ∷ LRTB Double
    , _margin          ∷ LRTB Double
    , _absolute        ∷ LRTB (Maybe Double)
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
    } → Geo
    deriving (Show)
makeLenses ''Geo

deriving instance Eq (LRTB Double)
deriving instance Eq (LRTB (Maybe Double))

data GeoFieldValue
  = GPadding             (LRTB Double)
  | GMargin              (LRTB Double)
  | GAbsolute            (LRTB (Maybe Double))
  | GJustifyContent      Alignment
  | GAlignContent        Alignment
  | GAlignItems          Alignment
  | GAlignSelf           Alignment
  | GPositioning         Positioning
  | GDirection           Direction
  | GWrap                Wrapping
  | GGrow                Int
  | GShrink              Int
  | GOrder               (Maybe Int)
  | GBasis               Double
  deriving (Show)

defGeo ∷ Geo
defGeo = Geo
  { _padding           = LRTB 0 0 0 0
  , _margin            = LRTB 0 0 0 0
  , _absolute          = LRTB Nothing Nothing Nothing Nothing
  , _justify'content   = AlignStart
  , _align'content     = AlignStretch
  , _align'items       = AlignStart
  , _align'self        = AlignAuto
  , _positioning       = Relative
  , _direction         = DirColumn
  , _wrap              = NoWrap
  , _grow              = 0
  , _shrink            = 1
  , _order             = Nothing
  , _basis             = 0
  }

defGeoDiff ∷ Geo → [GeoFieldValue]
defGeoDiff g =
  (   let x = _padding         g in [GPadding        x | x ≢ _padding         defGeo])
  <> (let x = _margin          g in [GMargin         x | x ≢ _margin          defGeo])
  <> (let x = _absolute        g in [GAbsolute       x | x ≢ _absolute        defGeo])
  <> (let x = _justify'content g in [GJustifyContent x | x ≢ _justify'content defGeo])
  <> (let x = _align'content   g in [GAlignContent   x | x ≢ _align'content   defGeo])
  <> (let x = _align'items     g in [GAlignItems     x | x ≢ _align'items     defGeo])
  <> (let x = _align'self      g in [GAlignSelf      x | x ≢ _align'self      defGeo])
  <> (let x = _positioning     g in [GPositioning    x | x ≢ _positioning     defGeo])
  <> (let x = _direction       g in [GDirection      x | x ≢ _direction       defGeo])
  <> (let x = _wrap            g in [GWrap           x | x ≢ _wrap            defGeo])
  <> (let x = _grow            g in [GGrow           x | x ≢ _grow            defGeo])
  <> (let x = _shrink          g in [GShrink         x | x ≢ _shrink          defGeo])
  <> (let x = _order           g in [GOrder          x | x ≢ _order           defGeo])
  <> (let x = _basis           g in [GBasis          x | x ≢ _basis           defGeo])

ppdefGeoDiff ∷ Geo → String
ppdefGeoDiff g = intercalate "." $ show <$> defGeoDiff g


-- * API
--
class Flex a where
  geo      ∷ Lens' a Geo
  size     ∷ Lens' a (Di (Maybe Double))
  children ∷ Lens' a [a]
  area     ∷ Lens' a (Area'LU Double)

child ∷ Flex a ⇒ Int → Traversal' a a
child n = children ∘ ix n

walk ∷ (Flex a, Monad m) ⇒ ([Int] → a → m ()) → a → m ()
walk action f = loop [0] f
  where loop trace f = do
          action trace f
          sequence $ zip (f^.children) [0..] <&>
            \(f', n) → loop (n:trace) f'
          pure ()

ppItemSize ∷ Flex a ⇒ a → String
ppItemSize x = T.unpack ∘ renderStrict ∘ layoutCompact $ pretty'mdi  $ x^.size

ppItemArea ∷ Flex a ⇒ a → String
ppItemArea x = T.unpack ∘ renderStrict ∘ layoutCompact $ pretty'Area'Int $ x^.area

showItemArea ∷ Flex a ⇒ a → String
showItemArea x = show $ x^.area

dump ∷ (Flex a, MonadIO m) ⇒ (Flex a ⇒ a → String) → a → m ()
dump ppf = walk
  (\trace f' → liftIO $ do
    putStrLn $ (concat $ take (length trace - 1) (repeat "  "))
      <> (show $ head trace) <> " " <> ppf f')


-- * Instances
--
-- instance Pretty a ⇒ Pretty (a) where
--   pretty = unreadable "" ∘ pretty'item

--
-- * no user-serviceable parts below, except for the 'layout' function at the very bottom.
--


pretty'mdouble ∷ Maybe Double → Doc ann
pretty'mdouble Nothing  = pretty '*'
pretty'mdouble (Just x) = pretty $ T.pack $ printf "%3d" (floor x ∷ Int)

pretty'mdi ∷ Di (Maybe Double) → Doc ann
pretty'mdi (Di (V2 ma mb)) = pretty'mdouble ma <> pretty ':' <> pretty'mdouble mb

pretty'item ∷ Flex a ⇒ a → Doc ann
pretty'item item = pretty @Text "Item size:"
  <>  pretty'mdi      (item^.size)
  <+> pretty'Area'Int (item^.area)


item'marginLT, item'marginRB ∷ Flex a ⇒ a → Vertical → Reverse → Double
item'marginLT item Vertical   Forward = item^.geo.margin.left
item'marginLT item Vertical   Reverse = item^.geo.margin.top
item'marginLT item Horisontal Forward = item^.geo.margin.top
item'marginLT item Horisontal Reverse = item^.geo.margin.left
item'marginRB item Vertical   Forward = item^.geo.margin.right
item'marginRB item Vertical   Reverse = item^.geo.margin.bottom
item'marginRB item Horisontal Forward = item^.geo.margin.bottom
item'marginRB item Horisontal Reverse = item^.geo.margin.right

item'size  ∷ Flex a ⇒ Major → Lens' a Double
item'size   (Major axis) = area ∘ area'b ∘ size'di ∘ di'd axis
item'size2 ∷ Flex a ⇒ Minor → Lens' a Double
item'size2  (Minor axis) = area ∘ area'b ∘ size'di ∘ di'd axis

item'pos   ∷ Flex a ⇒ Major → Lens' a Double
item'pos    (Major axis) = area ∘ area'a ∘ lu'po ∘ po'd axis
item'pos2  ∷ Flex a ⇒ Minor → Lens' a Double
item'pos2   (Minor axis) = area ∘ area'a ∘ lu'po ∘ po'd axis


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
    , _la'extra'flex'dim ∷ Double  -- sizes of flexible items
    , _la'flex'grows   ∷ Int
    , _la'flex'shrinks ∷ Int
    , _la'pos2         ∷ Double    -- minor axis position
    , _la'lines        ∷ [LayoutLine]
    , _la'lines'sizes  ∷ Double
    } → Layout
    deriving (Show)
makeLenses ''Layout

pretty'layout ∷ Layout → Doc ann
pretty'layout Layout{..} =
  let axes  = pretty $ T.pack (show $ fromMajor _la'major) <> ":" <> T.pack (show $ fromMinor _la'minor)
      wrap  = if _la'wrap then pretty @Text " Wrap" else mempty
      dir   = pretty $ T.take 4 $ T.pack $ show _la'vertical
      revMj = pretty $ (<>) "Maj" $ T.take 4 $ T.pack $ show _la'reverse
      revMi = pretty $ (<>) "Min" $ T.take 4 $ T.pack $ show _la'reverse2
  in pretty @Text "Layout " <> wrap <+> axes <+> dir <+> revMj <+> revMi
     <+> pretty ("par:" <> ppV2 (V2 _la'size'dim _la'align'dim))
     <+> pretty ("chi:" <> ppV2 (V2 _la'flex'dim _la'line'dim))
     <+> pretty ("efd:" <> T.pack (show _la'extra'flex'dim))
     <+> pretty ("g/s:"  <> ppV2 (V2 _la'flex'grows _la'flex'shrinks))
     <+> pretty @Text ("pos2:") <> pretty _la'pos2

instance Pretty Layout where
  pretty x = pretty '#' <> angles (pretty'layout x)


mkLayout ∷ Flex a ⇒ a → Layout
mkLayout item =
  let V2 width' height'  = item^.area.area'b.size'di.di'v
      Geo{..}            = item^.geo
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
      (,,,,)
        _la'flex'grows
        _la'flex'shrinks
        _la'flex'dim
        _la'extra'flex'dim
        _la'line'dim     = (,,,,) 0 0 0 0
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
  & la'extra'flex'dim .~ 0
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

count'relatives ∷ Flex a ⇒ [a] → Int
count'relatives = length ∘ filter ((≡Relative) ∘ (^.geo.positioning))

layout_items ∷ Flex a ⇒ a → [a] → Layout → (Layout, [a])
layout_items _             []       l            = (,) l []
layout_items item children l@Layout{..} =
  -- Determine the major axis initial position and optional spacing.
  let Geo{..}      = item^.geo
      flex'dim1         = _la'flex'dim + if _la'flex'dim > 0 ∧
                                            _la'extra'flex'dim > 0
                                         -- If the container has a positive flexible space, let's add to it the sizes of all flexible children.
                                         then _la'extra'flex'dim
                                         else 0
      (pos1, spacing1)  = if _la'flex'grows ≡ 0 ∧
                             flex'dim1 > 0
                          then let may'aligned = layout'align _justify'content flex'dim1 (count'relatives children) False
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
      l'                = (if _la'wrap ∧ _la'reverse2 ≡ Reverse -- line 454: l→wrap implied by l→reverse2
                           then l & la'pos2 -~ _la'line'dim
                           else l)
                          & la'flex'dim .~ flex'dim1
      layout'children ∷ Flex a ⇒ Double → [a] → [a] → (Double, [a])
      layout'children pos  []                                      acc = (,) pos (reverse acc)
      layout'children pos (c@((^.geo.positioning) → Absolute):rest) acc = layout'children pos rest (c:acc) -- Already positioned.
      layout'children pos (c:rest) acc =
        -- Grow or shrink the major axis item size if needed.
        let flex'size   = if | l'^.la'flex'dim > 0 ∧ c^.geo.grow   ≢ 0 → (l'^.la'flex'dim) ⋅ fromIntegral (c^.geo.grow)   / fromIntegral (l'^.la'flex'grows)
                             | l'^.la'flex'dim < 0 ∧ c^.geo.shrink ≢ 0 → (l'^.la'flex'dim) ⋅ fromIntegral (c^.geo.shrink) / fromIntegral (l'^.la'flex'shrinks)
                             | otherwise → 0
            child'size0 = if | l'^.la'flex'dim > 0 ∧ c^.geo.grow   ≢ 0 → 0
                             | otherwise → c ^. item'size _la'major
            c1          = c & item'size _la'major .~ child'size0 + flex'size
            -- Set the minor axis position (and stretch the minor axis size if needed).
            align'size  = c1 ^. item'size2 _la'minor
            c'align     = if c1^.geo.align'self ≡ AlignAuto
                          then item^.geo.align'items
                          else c1^.geo.align'self
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

layout_item ∷ ∀ a. Flex a ⇒ a → a
layout_item p@((^.children) → []) = p
layout_item p =
  let cstr   = p^.area.area'b
      assign'sizes ∷ [a] → Layout → [a] → [a] → (Layout, [a], [a])
      assign'sizes []                                       l            sized positioned = (,,) l (reverse sized) positioned
      assign'sizes (c@((^.geo.positioning) → Absolute):rest) l@Layout{..} sized positioned =
        let abs'size (Just val)  _           _          _   = val
            abs'size  Nothing   (Just pos1) (Just pos2) dim = dim - pos2 - pos1
            abs'size  _          _           _          _   = 0
            abs'pos  (Just pos1) _           _          _   = pos1
            abs'pos   Nothing   (Just pos2) (Just size) dim = dim - size - pos2
            abs'pos   _          _           _          _   = 0
            (pos, dim) = (c^.geo.absolute, c^.size)
            c' = c & area .~ Area (LU   $ Po $ V2 (abs'pos  (pos^.left) (pos^.right) (dim^.di'd X) (cstr^.size'di.di'd X))
                                                  (abs'pos  (pos^.top)  (pos^.bottom) (dim^.di'd Y) (cstr^.size'di.di'd Y)))
                                  (Size $ Di $ V2 (abs'size (dim^.di'd X) (pos^.left) (pos^.right) (cstr^.size'di.di'd X))
                                                  (abs'size (dim^.di'd Y) (pos^.top) (pos^.bottom) (cstr^.size'di.di'd Y)))
                   & layout_item
        in assign'sizes rest l (c':sized) positioned
      assign'sizes (c:rest) l@Layout{..} sized positioned =
        let c'size  = fromMaybe 0 $ partial (>0) (c^.geo.basis) <|>
                                                 (c^.size.di'd (fromMajor _la'major))
            c'size2 = flip fromMaybe (c^.size.di'd (fromMinor _la'minor))
                                     (cstr^.size'di.di'd (if _la'vertical ≡ Vertical then X else Y) -
                                      item'marginLT c _la'vertical Forward -
                                      item'marginRB c _la'vertical Forward)
            -- NB: self_sizing callback implementation ignored
            (,,) sized' positioned' l'
              = if not _la'wrap ∨ l^.la'flex'dim ≥ c'size then (,,) sized positioned l
                else let (,) lay' positioned' = layout_items (p) (reverse sized) l
                     in (,,) [] (positioned <> positioned') (layout'reset lay')
            l'' = l' & la'line'dim %~ (\line'dim→ if not _la'wrap ∨ c'size2 ≤ line'dim then line'dim
                                                  else c'size2)
                     & la'flex'grows   +~ c'^.geo.grow
                     & la'flex'shrinks +~ c'^.geo.shrink
                     & la'flex'dim     -~ c'size + item'marginLT c' _la'vertical Reverse
                                                 + item'marginRB c' _la'vertical Reverse
                     & la'extra'flex'dim +~ if c'size > 0 ∧ c'^.geo.grow > 0
                                            then c'size
                                            else 0
            c' = c & item'size  _la'major .~ c'size
                   & item'size2 _la'minor .~ c'size2
        in assign'sizes rest l'' (c':sized') positioned'
      -- 1. Assign sizes (and some of positions, if wrapping):
      (,,) lay       sized positioned  = assign'sizes (p^.children) (layout'reset $ mkLayout p) [] [] ∷ (Layout, [a], [a])
      -- 2. Assign remainder of positions:
      (,)  lay'@Layout{..} positioned' = layout_items p sized lay
      -- 3. Merge wrap-generated positionals & the results of remainder processing:
      children'  = positioned <> positioned'
      -- 4. In wrap mode if the 'align_content' property changed from its default
      --    value, we need to tweak the position of each line accordingly.
      align'children ∷ Layout → [a] → [a]
      align'children Layout{..} cs =
        let flex'dim          = _la'align'dim - _la'lines'sizes
            may'aligned       = layout'align (p^.geo.align'content) flex'dim (length _la'lines) True
            (,) pos' spacing' = if flex'dim ≤ 0 then (,) 0 0
                                else flip fromMaybe may'aligned $ error "incorrect align_content"
            (,) pos'' old'pos = if _la'reverse2 ≢ Reverse then (,) pos' 0
                                else (,) (_la'align'dim - pos') _la'align'dim
            line'step ∷ [LayoutLine] → [a] → [[a]] → Double → Double → ([a], Double, Double)
            line'step [] []     acc pos old'pos = (,,) (concat $ reverse acc) pos old'pos
            line'step [] (u:us) _   _   _       = error $ printf "Invariant failed: %d children left unaccounted for." (length us + 1)
            line'step (LayoutLine{..}:ls) cs acc pos old'pos =
              let (,) pos' old'pos'   = if _la'reverse2 ≢ Reverse then (,) pos  old'pos
                                        else (,) (pos - _li'size - spacing') (old'pos - _li'size)
                  (,) line rest       = splitAt _li'nchildren cs
                  -- Re-position the children of this line, honoring any child alignment previously set within the line
                  line'               = line <&> \c→ if c^.geo.positioning ≡ Absolute then c
                                                     else c & item'pos2 _la'minor +~ pos' - old'pos'
                  (,) pos'' old'pos'' = if _la'reverse2 ≡ Reverse then (,) pos' old'pos'
                                        else (,) (pos + _li'size + spacing') (old'pos + _li'size)
              in line'step ls rest (line':acc) pos'' old'pos''
        in (^._1) $ line'step (reverse _la'lines) cs [] pos'' old'pos
  in p & children .~ if _la'wrap ∧ p^.geo.align'content ≢ AlignStart
                     then align'children lay' children'
                     else children'


-- | Lay out children according to their and item's properties.
--   Origin is fixed to 0:0.
layout ∷ Flex a ⇒ Size Double → a → a
layout dim x = layout_item $
  x & area.area'b .~ dim
