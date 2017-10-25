--{-# OPTIONS_GHC -fplugin GHC.TypeLits.Extra.Solver #-}
--{-# OPTIONS_GHC -fplugin GHC.TypeLits.Normalise #-}
{-# LANGUAGE ConstraintKinds, DataKinds, KindSignatures, TypeApplications, TypeInType #-}
{-# LANGUAGE FlexibleContexts, FlexibleInstances, InstanceSigs, MultiParamTypeClasses, NoMonomorphismRestriction, RankNTypes, TypeSynonymInstances, UndecidableInstances #-}
{-# LANGUAGE GADTs, FunctionalDependencies, TypeFamilies, TypeFamilyDependencies #-}
{-# LANGUAGE BangPatterns, LambdaCase, MultiWayIf, OverloadedStrings, RecordWildCards, ScopedTypeVariables, StandaloneDeriving, TypeOperators, UnicodeSyntax, ViewPatterns #-}
{-# LANGUAGE DeriveFoldable, DeriveFunctor, DeriveGeneric, GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wall -Wno-unticked-promoted-constructors -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
module Space where

-- Basis
import           Prelude                           hiding (putStrLn)
import           Prelude.Unicode

-- Type-level
import           GHC.Generics                             (Generic)
import           GHC.TypeLits
import           GHC.Types                         hiding (Constraint)

-- General types
import           Control.Applicative
import           Control.Applicative.Free
import           Control.Lens                      hiding (children)
import           Control.Monad.Random              hiding (lift, void)
import           Control.Monad.State               hiding (lift, void)
import           Data.Complex
import           Data.Function
import           Data.List
import           Data.Lub
import qualified Data.Map                          as Map
import           Data.Maybe
import           Data.Glb
import           Data.MeasuredMonoid
import           Data.MonoTraversable
import           Data.Monoid
import qualified Data.Text                         as T
import           Data.Text                                (Text)
import           Data.Text.Lazy                           (toStrict)
import           Data.Text.IO
import           Data.Type.Bool
import           Data.Void                                (Void)

-- Algebra
import           Linear                            hiding (trace)

-- glib-introspection -based Pango
import qualified GI.Pango                          as GIP (unitsToDouble, unitsFromDouble)

-- Misc
import           Debug.Trace                              (trace)
import           Data.Text.Format                         (format, Only(..))
import qualified Data.Text.Format                  as T

-- Dirty stuff
import qualified Foreign                           as F
import qualified System.IO.Unsafe                  as UN

-- Local
import           Elsewhere
import           Flatland


-- * Narrative
--
-- ** R, for design Requirements
--    1. We want to organize an simplified, constraint-solving workflow -- a 80% solution, indeed.
--    2. UI scaling is a concern we can't afford to ignore in 2017.
--    3. We want to encapsulate the solution of UI scaling concern inside the language of Spaces.
--
-- ** M, for iMplementation requirements
--    1. A free applicative is desired as the embodiment of ∈ent's visual layout, because:
--       - it lies in a sweet spot of expressivity, between Functor and Monad:
--         - entire tree can be analyzed prior to effect execution, unlike with Monad
--         - said analysis can be performed under multiple interpretations
--         - it provide a needed expressivitu boost over Functor
--       - it provides a solid abstraction, isolating layered types from each other
--    2. A GADT is needed to facilitate type-level propagation of information
--
-- ** C, for Immediate conclusions
--    1. R3 can't be resolved, unless object size requirements are scale/context sensitive.
--    2, M1 → a Functor instance is mandatory, by definition of free applicative.
--
-- ** D, for deductions
--    1. C1 → context sensitivity requires a function of form (a → Context → Size)
--    2. Type classes cannot provide D1, because..
--       - there is no obvious location to provide the constraint for C1,
--         because C2 requires unrestrained polymorphism on the variable of 'a'.
--       - skolem, implied by free applicative?  This is admittedly less clear, because of
--         well, the way we choose to use FA.
--         - we should elucidate the reasoning here..
--    3. D2 → the only other option is explicit provision of such a type-dependent method vocabulary
--

-- * TODO
--
-- 1. Tracking the dimensionality type across the entire thing is painful --
--    we might want to try shifting this information into a promoted type.
--
--    We'd still need to deal with the kind equality, but there's a hunch that
--    this might be easier.
--

-- * Temporary polymorphism reduction
--
type FixedUnit = Double


-- * Requirement description
--
type Constraint = Maybe (Cstr FixedUnit)

data Requirement where
  Requirement ∷
    { _hard ∷ Maybe (Reqt FixedUnit) -- ^ Absolutely-dimensioned (screen units)
    , _soft ∷ Maybe (Reqt FixedUnit) -- ^ Relatively-dimensioned (screen ratio)
    , _eff  ∷ Maybe (Reqt FixedUnit) -- ^ Absolutely-dimensioned, as it's concrete
    } → Requirement
    deriving (Show)
makeLenses ''Requirement

reqm'has'hard, reqm'has'soft, reqm'has'eff ∷ Requirement → Bool
reqm'has'hard = isJust ∘ view hard
reqm'has'soft = isJust ∘ view soft
reqm'has'eff  = isJust ∘ view eff

type Origin = Maybe (Orig FixedUnit)

instance Monoid Requirement where
  mempty = Requirement Nothing Nothing Nothing


-- * Screen-space dimensions and requirement querying
--
newtype ScreenConstr = ScreenConstr { _scrc'cstr ∷ Cstr FixedUnit } deriving (Eq, Show)
makeLenses ''ScreenConstr

data UnitI
  = UDouble
  | UInt

scrc'v ∷ Lens' ScreenConstr (V2 FixedUnit)
scrc'v = scrc'cstr ∘  cstr'v
scrc'd = scrc'cstr .: cstr'd

class Requires a where
  hard'requires ∷ ScreenConstr → a → Maybe (Reqt FixedUnit)
  soft'requires ∷                a → Maybe (Reqt FixedUnit)
  --
  hard'requires _ _ = Nothing
  soft'requires   _ = Nothing

-- requires ∷ Requires a ⇒ ScreenConstr (ReqDimen a) → a → Requirement (ReqDimen a)
requires ∷ Requires a ⇒ ScreenConstr → a → Requirement
requires c x = Requirement (hard'requires c x) (soft'requires x) Nothing


-- * Space ~ (Constraint * Requirement * Origin)
--
data Space' (d ∷ Type) where
  Space ∷
    { _cstr ∷ Constraint
    , _reqt ∷ Requirement
    , _orig ∷ Origin
    } → Space' d
  deriving (Show)
makeLenses ''Space'

type Space = Space' FixedUnit

empty'space ∷ Space
empty'space = Space Nothing mempty Nothing

prettySpace ∷ Space' d → Doc
prettySpace (Space c r s o) =
        prettyMaybe "*" ((text "Cstr" <:>) ∘ text ∘ ppV2  ∘ (^.cstr'v) <$> c)
    <-> prettyMaybe "*" (prettyR ∘ (^.rp'min) <$> r)
    <-> prettyMaybe "*" (prettyR ∘ (^.rp'opt) <$> r)
    <-> prettyMaybe "*" ((text "Size" <:>) ∘ text ∘ ppV2  ∘ (^.reqt'v) <$> s)
    <-> prettyMaybe "*" ((text "Orig" <:>) ∘ text ∘ ppV2  ∘ (^.orig'v) <$> o)
    where prettyR (Reqmt ty req) = pretty ty <:> text (ppV2 $ req^.reqt'v)
class IsSpace t where
  liftSp ∷ t → Space

instance Pretty (Space' d) where
  pretty = unreadable "Space" ∘ prettySpace

-- * Space constructors

trace'space ∷ Space → Space
trace'space x = trace (ppCompactS x) x
mk'hardReq ∷ Reqt FixedUnit → Requirement
mk'hardReq x = Requirement (Just x) Nothing Nothing

instance IsSpace Constraint  where liftSp x = empty'space & cstr .~ x
instance IsSpace Origin      where liftSp x = empty'space & orig .~ x
instance IsSpace Requirement where liftSp x = empty'space & reqt .~ x

s'beside ∷ Space → Orient Card → Space → Space
s'beside (Space _ (Requirement _ _ (Just r)) (Just o)) at what@(Space _ (Requirement _ _ (Just r')) _) =
  what & orig .~ (Just $ orig'beside at o r r')

s'inside ∷ Space → Orient a → Space → (Space, Space)
s'inside = (⊥)
-- * Questions for type-driven subsetting:
--     1. orientations we can handle
--     2. spaces we can handle


-- * If we'd ever need a Place, it's here:
--
data Place d where
  Nowhere ∷ Place d
  Center ∷
    { _pCoord ∷ !(Po d)
    } → Place d
  Port ∷
    { _pCoord ∷ !(Po d)
    , _pPort  ∷ !(Orient a)
    } → Place d
makeLenses ''Place
-- ^
-- A Monoid instance seems impossible for Place,
-- due to unavailability of dimensions.
--
-- Such is the genesis of the simpler Origin type.
--
-- The need for a Monoid instance basically forces us
-- to standartize upon a point of origin.
--
-- And it better be Center.
--
deriving instance Show d ⇒ Show (Place d)

-- origin'place ∷ Origin d → Place d
-- origin'place Nothing  = Nowhere
-- origin'place (Just x) = Center x



-- * CA: free applicative that contextualises child nodes with:
--   1. Space
--   2. Structure (C)
--
data Kind       = Obj | Align | Constr | Req | Wrap | Grav | HBox | VBox | Grid | FBox
data KArity     = One | Many
data KSize      = SzCons | SzReq | NoSize
data KPosition  = Abs | Rel | NoPos

instance Pretty (C' d a) where
  pretty (C sp CObj{..})  = unreadable "Obj"         $ nest 8 $ prettySpace sp
  pretty (C sp CBox{..})  = unreadable
                            (showTL _caxes <> "Box") $ nest 8 $ prettySpace sp <> softline <> (vcat $ pretty <$> _cs)
  pretty (C sp CWrap{..}) = unreadable "Wrap"        $ nest 8 $ prettySpace sp <> softline <+> pretty _cw

instance Pretty (Ap (C' d) a) where
  pretty = runAp_ pretty

-- ca'requires ∷ (Lin d, Requires (C a) d) ⇒ ScreenConstr d → Ap (C d) a → Requirement d
-- ca'requires scrc x = runAp_ (requires scrc) x

ca'cstr ∷ Ap C a → Constraint
ca'cstr = runAp_ (^.space.cstr)
ca'reqt ∷ Ap C a → Requirement
ca'reqt = runAp_ (^.space.reqt)
ca'orig ∷ Ap C a → Origin
ca'orig = runAp_ (^.space.orig)

ca'set'orig ∷ Origin → Ap C a → Ap C a
ca'set'orig v = hoistAp (& space.orig .~ v)


-- * Structure
--
-- data CF d a where
--   CF ∷ Show a ⇒
--     { cf'spc    ∷ Space d
--     , cf'obj    ∷ C d k (Ap (C d) a)
--     } → CF d a

space    ∷ Lens' (C a) Space
space    f c               = fmap (\s'  -> c { _space = s' })  (f $ _space c)

children ∷ Lens' (C a) [Ap C a]
children f c@(CHBox _ _) = fmap (\cs' -> c { _chs   = cs' }) (f $ _chs c)
children f c@(CVBox _ _) = fmap (\cs' -> c { _cvs   = cs' }) (f $ _cvs c)
children _ _ = error "Misapplication of a 'children' lens to a wrong GADT constructor.  Please convince author to go type-level."

child ∷ Lens' (C a) (Ap C a)
child f c@(CWrap _ _ _ _)  = fmap (\c'  -> c { _cw    = c' })  (f $ _cw c)
child _ _ = error "Misapplication of a 'children' lens to a wrong GADT constructor.  Please convince author to go type-level."

data    C' d a where
  CObj ∷ Requires a ⇒
    { _space   ∷ Space
    , _cobj    ∷ a
    } → C' d a
  CHBox ∷ Requires a ⇒
    { _space   ∷ Space
    , _chs     ∷ [Ap (C' d) a]
    } → C' d a
  CVBox ∷ Requires a ⇒
    { _space   ∷ Space
    , _cvs     ∷ [Ap (C' d) a]
    } → C' d a
  CWrap ∷ Requires a ⇒
    { _space   ∷ Space
    , _cwNW    ∷ !(Di d) -- ^ The combined offsets _ the left and top sides.
    , _cwSE    ∷ !(Di d) -- ^ The combined offsets _ the right and bottom sides.
    , _cw      ∷ Ap (C' d) a
    } → C' d a
    deriving ()

deriving instance (Show a, Show d) ⇒ Show (S' d a)
deriving instance (Show a, Show d) ⇒ Show (C' d a)
instance (Show a, Show d) ⇒ Show (Ap (C' d) a) where
  show = runAp_ $ with'C'dicts show
type C = C' FixedUnit

with'C'Requires ∷ (∀ b. (b ~ a, Requires b) ⇒ C b → c) → C a → c
with'C'Requires f x = x & case x of
  CObj      _ _ → f
  CVBox     _ _ → f
  CHBox     _ _ → f
  CWrap _ _ _ _ → f

-- makeLenses ''C
  -- -- CRel ∷
  --   { _cRel    ∷ !(Po d)
  --   , _crela   ∷ a
  --   } → C d      Pos        One          NoSize      Rel        a
  -- CAlign ∷
  --   { _cAlign  ∷ !(Orient Card)
  --   , _calia   ∷ [a]
  --   } → C d      Align      One          NoSize      NoPos      a
  -- CGrav ∷
  --   { _cas     ∷ ![(Gravity, a)]
  --   } → C d      Grav       One          NoSize      NoPos      a
  -- CFBox ∷
  --   { _cvAlloc ∷ ![Th d] -- ^ Relative allocations
  --   , _cvs     ∷ [a]
  --   } → C d      FBox       Many         SzCons      NoPos      a


-- * Description language
--
-- Note: we're mostly starting un-spaced, where appropriate.
--
lift ∷ (Requires a) ⇒ Reqt FixedUnit → a → Ap C a
lift = liftAp .: CObj ∘ liftSp ∘ mk'hardReq

hbox, vbox ∷ (Requires a) ⇒ [Ap C a] → Ap C a
hbox = liftAp ∘ CHBox empty'space
vbox = liftAp ∘ CVBox empty'space


instance Requires Double where

-- hb ∷ Lin (ReqDimen a) ⇒ Ap (C (ReqDimen a)) a
hb =
  hbox
  [ lift (Reqt $ di 10 10) 0
  , lift (Reqt $ di 10 10) 1
  ]

demo ∷ Ap C Double
demo =
  vbox
  [ lift (Reqt $ di 10 10) 0
  , hb
  ]

demo' ∷ Ap C Double
demo' =
  let f = assign'eff'requires (ScreenConstr ∘ Cstr $ di 10 10)
  in hoistAp (with'C'Requires f) demo



-- * Analyses for CA
--
-- * HBox/VBox requirement model
--
-- reqmt'add  ∷ Lin d ⇒ Axes → Requirement d → Requirement d → Requirement d
-- reqmt'add _   l                       Nothing               = l
-- reqmt'add _   Nothing                r                      = r
-- reqmt'add XY (Just (Di (V2 lx ly))) (Just (Di (V2 rx ry))) = Just ∘ Di $ V2 (lx   +   ly) (rx   +   ry)
-- reqmt'add X  (Just (Di (V2 lx ly))) (Just (Di (V2 rx ry))) = Just ∘ Di $ V2 (lx   +   ly) (rx `max` ry)
-- reqmt'add  Y (Just (Di (V2 lx ly))) (Just (Di (V2 rx ry))) = Just ∘ Di $ V2 (lx `max` ly) (rx   +   ry)

-- reqmt'add'xy ∷ Lin d ⇒ Requirement d → Requirement d → Requirement d
-- reqmt'add'xy = reqmt'add XY

-- , ctx'origin  = Just $ (/2) $ fromMaybe zero (origin  l) + fromMaybe zero (origin  r)


-- * Requirements
--
-- propagate'hard'soft'requires ∷ Requires a ⇒ ScreenConstr → C a → C a
-- propagate'hard'soft'requires

assign'eff'requires ∷ Requires a ⇒ ScreenConstr → C a → C a

-- If we already have an effective requirement, there's nothing to do
assign'eff'requires _ x@(isJust ∘ (^. space.reqt.hard) → True) = x

assign'eff'requires _      (CObj  (Space  Nothing   _                         _) _) = error "Asked to assign requires to an object without constraints."
assign'eff'requires _    x@(CObj  (Space (Just _)  (Requirement _ _ (Just _)) _) _) = x -- already has an effective requirement
assign'eff'requires scrc x@(CObj  (Space (Just _)  (Requirement _ _  Nothing) _) o) = x & space.reqt .~ requires scrc o

assign'eff'requires _    x@(CHBox (Space (Just cst) _                         _) _) = assigh'eff'requires'box x cst X
assign'eff'requires _    x@(CVBox (Space (Just cst) _                         _) _) = assigh'eff'requires'box x cst Y

assigh'eff'requires'box ∷ C a0 -> Cstr FixedUnit -> Axes -> C a0
assigh'eff'requires'box x x'cstr axis =
  let -- Query children's soft/hard requirements
      chi'allRs           = ca'reqt <$> x^.children
      partition'by'hardness xs acc@(ds, ss)
        | []      ← xs = acc
        | (x:xs') ← xs = partition'by'hardness xs'
                         $ case x of
                             Requirement (Just d) _ _ → (d:ds, ss)                 -- XXX: ignores a soft requirement, when there's a soft one
                             Requirement _ (Just s) _ → (ds, s:ss)
                             _                        → (ds, mempty:ss)            -- XXX: interprets no requirement at all as zero requirement
      (,) chi'hardRs
          chi'softRs   = partition'by'hardness chi'allRs ([], [])
      chi'hardR'sum    = foldr (reqt'add axis) mempty chi'hardRs                   -- XXX: softs are ignored wrt. the secondary axes
      abs'remains      = x'cstr^.cstr'd axis - chi'hardR'sum^.reqt'd axis
      soft'abs'ratio   = abs'remains / (sum $ (^.reqt'd axis) <$> chi'softRs)
      -- Compute a child's constraint by promoting its own requirement
      req'cstr (Requirement (Just hardR) _        _) =  Cstr $ Di $ hardR^.reqt'v
      req'cstr (Requirement _        (Just softR) _) = (Cstr $ Di $ softR^.reqt'v) & cstr'd axis %~ (* soft'abs'ratio)
      req'cstr (Requirement _        _            _) = error "req'cstr called on a bad Requirement."
      -- Assign constraints
      chis             = [ hoistAp (& space.cstr._Just .~ req'cstr req) c
                         | (req, c) ← zip chi'allRs (x^.children) ]
  in x & space.reqt.eff._Just.reqt'v .~ x'cstr^.cstr'v                             -- SIMPLISTIC?: slap our own constraint as the requirement
       & children                    .~ chis

-- assign'eff'requires scrc x@CWrap{..} =
--   let chi       = runAp (liftAp ∘ assign'eff'requires) _cw
--       chi_req   = ca'reqt chi
--       req       = (mk'reqmt _cwNW) `reqmt'add'xy` chi_req `reqmt'add'xy` (mk'reqmt _cwSE)
--   in x & space.reqt .~ req
--        & child         .~ chi

foldilate ∷ (a → s → (c, s)) → s → [a] → ([c], s)
foldilate f s xs = runState (mapM f' xs) s
  where f' x = do
          (x', state'') ← f x <$> get
          put state''
          pure x'

space'op ∷ Space → [C a] → (C a → State Space (C a)) → ([C a], Space)
space'op spc xs f = runState (mapM f xs) spc

cursor'state'op ∷ Po d → [C a] → (C a → State (Po d) (C a)) → ([C a], Po d)
cursor'state'op y xs f = runState (mapM f xs) y


-- * Origination
--
-- NOTE: this is based on Origin = Center
assign'origins ∷ C a → C a
-- * REQUIREMENTS
--
-- For this code we want something more high-level than banging Di's against Po's.
-- This implies, as a first guess:
--  - ability to put things beside each other
--    - Orients
--    - a combination of Di and Po in the combinator
--
assign'origins   (_space → (Space _ _ Nothing)) = error "Attempt to assign origins to children of an unoriginated tree."

assign'origins x@CObj{..} = x
  -- x { _space = _space { s'orig = ca'origin _cobj } }

-- assign'origins x@(CHBox s (_chs ∷ [Ap (C d) a])) =
--   let (origd, _) = runState (mapM f _chs) (s^.s'orig)
--       f chi      = do
--         cursor ← get
--         put $ cursor -- want something less stupid than manual shoving of Di's and Po's
--         pure $ ca'set'origin cursor chi
--   in x & children .~ origd

-- assign'origins x@CVBox{..} =
--   let chis      = runAp (liftAp ∘ assign'eff'requires) <$> _cvs
--       req       =
--       -- foldl' (reqmt'add Y) (s'reqmt _space) (ca'reqmt <$> chis)
--   in x { _space = _space { s'reqmt = req }
--        , _chs   = chis }

-- assign'origins x@CWrap{..} =
--   x & space.s'orig .~ ca'origin _cw


data Gravity where
  Centre ∷               Gravity
  Corner ∷ Orient Corn → Gravity
  Side   ∷ Orient Card → Gravity
  deriving (Eq, Show)


-- deriving instance Show u ⇒ Show (S k p u)
-- instance Functor (S Area False) where fmap f (Farea x)       = Farea (fmap f x)
-- instance Functor (S Area True)  where fmap f (Parea x y)     = Parea (fmap f x) (fmap f y)
-- instance Functor (S Wrap False) where fmap f (Fwrap x y)     = Fwrap (fmap f x) (fmap f y)
-- instance Functor (S Wrap True)  where fmap f (Pwrap x y z w) = Pwrap (fmap f x) (fmap f y) (fmap f z) (fmap f w)
-- instance Applicative (S Area False) where
--   pure x = Farea $ pure x
--   Farea x <*> Farea y = Farea $ x <*> y
-- instance Applicative (S Area True)  where
--   pure x = Parea (pure x) (pure x)
--   Parea x x' <*> Parea y y' = Parea (x <*> y) (x' <*> y')
-- instance (Ord a) ⇒ HasLub (S Area False a) where lub = Farea .: on lub _aD
-- instance (Ord a) ⇒ HasGlb (S Area False a) where glb = Farea .: on glb _aD
-- instance (Num a, Ord a) ⇒ HasGlb (S Area True a) where
--   Parea _ p `glb` Parea _ p' = Parea (poDelta lu gl) gl
--     where gl = glb p p'
--           lu = lub p p'


-- * Constructors
-- class Num a ⇒ AspectS a k where aspect ∷ Di a → S k False a
-- class Num a ⇒ SymmS   a k where symm   ∷ Th a → S k False a
-- class Num a ⇒ GoldXS  a k where goldX  ∷ Wi a → S k False a
-- class Num a ⇒ GoldYS  a k where goldY  ∷ He a → S k False a
-- instance Num a ⇒      AspectS a Area where aspect =      Farea
-- instance Num a ⇒      AspectS a Wrap where aspect = join Fwrap
-- instance Num a ⇒      SymmS   a Area where symm   =      Farea ∘ Di ∘ join V2 ∘ _thV
-- instance Num a ⇒      SymmS   a Wrap where symm   = join Fwrap ∘ Di ∘ join V2 ∘ _thV
-- instance RealFrac a ⇒ GoldXS  a Area where goldX  =      Farea ∘ goldXdi
-- instance RealFrac a ⇒ GoldXS  a Wrap where goldX  = join Fwrap ∘ goldXdi
-- instance RealFrac a ⇒ GoldYS  a Area where goldY  =      Farea ∘ goldYdi
-- instance RealFrac a ⇒ GoldYS  a Wrap where goldY  = join Fwrap ∘ goldYdi

-- instance (Num a, Random a) ⇒ Random (S Area True a) where
--   random = runState $ Parea <$> (state random) <*> (state random)
--   randomR (Parea di' nw, Parea maxsz _) =
--     let se = poAdd nw ∘ _diV $ di' ^-^ maxsz
--     in runState $ liftA2 Parea (state $ randomR (zero, maxsz)) (state $ randomR (nw, se))


-- * Projections
--
-- pareaSE ∷ Num a ⇒ S Area True a → Po a
-- pareaSE (Parea di' po') = poAdd po' $ _diV di'

-- dim ∷ Num a ⇒ S k p a → Di a
-- dim Farea{..} = _aD
-- dim Parea{..} = _paD
-- dim Fwrap{..} = _wNWd ^+^ _wSEd
-- dim Pwrap{..} = Di ∘ _poV $ _pwSEp ^-^ _pwNWp

-- -- | 'wThL', 'wThT', 'wThR', 'wThB' -- sidewise dimensions.
-- wThL, wThR ∷ S Wrap p a → Wi a
-- wThT, wThB ∷ S Wrap p a → He a
-- wThL Fwrap{..} = Wi ∘ (view _x) $ _diV _wNWd; wThL Pwrap{..} = Wi ∘ (view _x) $ _diV _pwNWd
-- wThT Fwrap{..} = He ∘ (view _y) $ _diV _wNWd; wThT Pwrap{..} = He ∘ (view _y) $ _diV _pwNWd
-- wThR Fwrap{..} = Wi ∘ (view _x) $ _diV _wSEd; wThR Pwrap{..} = Wi ∘ (view _x) $ _diV _pwSEd
-- wThB Fwrap{..} = He ∘ (view _y) $ _diV _wSEd; wThB Pwrap{..} = He ∘ (view _y) $ _diV _pwSEd


-- * Combinators
--
-- area ∷ Num a ⇒ S k True a → S Area True a
-- area a@Parea{..} = a
-- area (Pwrap _ _ (Po nw) (Po se)) =
--   Parea (Di $ se ^-^ nw) (Po nw)

-- areaNarrow ∷ Num a ⇒ Th a → S Area p a → S Area p a
-- areaNarrow (Th d) (Farea (Di a))        = Farea (Di $ a ^-^ (V2 d d) ^* 2)
-- areaNarrow (Th d) (Parea (Di a) (Po p)) = Parea (Di $ a ^-^ (V2 d d) ^* 2) (Po $ p ^+^ (V2 d d))


-- * Pinning
--
-- | Make pwNWp and pwSEp the top-leftmost and bottom-rightmost pixels of the 'Wrap'.
--   Warning:  no check on whether the coordinates are compatible with the dimensions is performed.
-- pinArea ∷ S Area False a → Po a → S Area True a
-- pinArea Farea{..} _paNWp = Parea{..}
--   where _paD = _aD
-- pinWrap ∷ S Wrap False a → Po a → Po a → S Wrap True a
-- pinWrap Fwrap{..} _pwNWp _pwSEp = Pwrap{..}
--   where _pwNWd = _wNWd
--         _pwSEd = _wSEd



-- | Space partitioning:
--
-- data Space (pinned ∷ Bool) a (n ∷ Nat) where
--   End   ∷ Num a ⇒                Space p a 0
--   Sarea ∷ Num a ⇒
--     { sArea  ∷ !(S Area p a) } → Space p a 1
--   Spc   ∷
--     (Num a, n ~ (m + 1)) ⇒
--     { sWrap  ∷ !(S Wrap p a)
--     , sInner ∷  Space p a m }  → Space p a n

-- deriving instance Show a ⇒  Show (Space p a n)

-- instance (Num a, Show a) ⇒ MeasuredMonoid (Space p a) where
--   mmempty   = End
--   mmappend    End         End       = End
--   mmappend    End       x@Sarea{..} = x
--   mmappend    End       x@Spc{..}   = x
--   mmappend  x@Sarea{..}   End       = x
--   mmappend  x@Sarea{..}   tail'     = error $ printf "Sarea `mmappend` non-End is ⊥: %s to %s" (show x) (show tail')
--   mmappend  x@Spc{..}     End       = x
--   mmappend   (Spc pl nl)  tail'     = Spc pl $ mmappend nl tail'

-- mkSpace ∷ Fractional a ⇒ Di a → Space False a 1
-- mkSpace = Sarea ∘ Farea

-- spaceGrow      ∷ (Num a) ⇒ Th a → Space False a d → Space False a (d + 1)
-- spaceGrow        δ sp = Spc (symm  $ δ) sp
-- spaceGrowGoldX ∷ (RealFrac a) ⇒ Wi a → Space False a n → Space False a (n + 1)
-- spaceGrowGoldX   δ sp = Spc (goldX $ δ) sp
-- spaceGrowGoldY ∷ (RealFrac a) ⇒ He a → Space False a n → Space False a (n + 1)
-- spaceGrowGoldY   δ sp = Spc (goldY $ δ) sp

-- | Compute the total allocation for a 'Space'.
--   Complexity: O(depth) for un-pinned, O(1) for pinned.
-- spaceDim ∷ Space p u d → Di u
-- spaceDim  End                           = zero
-- spaceDim (Sarea a)                      = dim a
-- spaceDim (Spc w@(Fwrap  _ _)    sInner) = dim w ^+^ spaceDim sInner
-- spaceDim (Spc w@(Pwrap _ _ _ _) _)      = dim w


-- * Pinning

-- | Compute the SE point for an un-pinned 'Space', given its NW point.
--   Complexity: O(depth).
-- spaceSE  ∷ Space False a d → Po a → Po a
-- spaceSE  s@Spc{..} lt = Po $ _poV lt ^+^ _diV (spaceDim s) --  ^-^ V2 1 1

-- | Pin space to the @lt
-- spacePin ∷ Num a ⇒ Po a → Space False a n → Space True a n
-- spacePin lt space = loop space zero rb
--   where rb = spaceSE space lt
--         loop ∷ Space False a n → Po a → Po a → Space True a n
--         loop  End                     _  _ = End
--         loop (Sarea Farea{..})       lt' _ =
--           Sarea (Parea { _paD   = _aD,                   _paNWp = lt' })
--         loop (Spc Fwrap{..} swInner) lt' rb' =
--           Spc   (Pwrap { _pwNWd = _wNWd, _pwSEd = _wSEd, _pwNWp = lt', _pwSEp = rb' })
--           $   loop swInner
--               (lt' ^+^ (Po ∘ _diV) _wNWd)
--               (rb' ^-^ (Po ∘ _diV) _wSEd)

-- x = let f1 = (ELit (2 :: Rational)) .*. EVar "x1" .+. EVar "x2" .+. EVar "x3" .<=. ELit 14
--         f2 = ELit (4 :: Rational) .*. EVar "x1" .+. ELit (2 :: Rational) .*. EVar "x2"
--              .+. ELit (3 :: Rational) .*. EVar "x3" .<=. ELit 28
--         f3 = ELit (2 :: Rational) .*. EVar "x1" .+. ELit (5 :: Rational) .*. EVar "x2"
--              .+. ELit (5 :: Rational) .*. EVar "x3" .<=. ELit 30
--         obj = EVar "Z" .==. EVar "x1" .+. ELit (2 :: Rational) .*. EVar "x2"
--               .+. ELit (-1 :: Rational) .*. EVar "x3"
--         t@(Tableau _ (c_s,_),_) = simplexPrimalRational
--           (makeRestrictedTableau [f1,f2,f3], unEquStd $ standardForm obj)
--     in basicFeasibleSolution c_s


--- Not sure if needed
---

-- areaDi :: Lens' (S Area p a) (Di a)
-- areaDi f Farea{..} = Farea <$> f _aD
-- areaDi f Parea{..} = flip Parea _paNWp <$> f _paD

-- center ∷ Fractional a ⇒ S k True a → Po a
-- center (Parea d p)                 = poAdd ∘ _diV $ p (d ^/ 2)
-- center (Pwrap _ _ (Po nw) (Po se)) = Po $ (se ^+^ nw) ^/ 2

-- -- | XXX/Lensify: update the wrap of the innermost space
-- mapSpaceArea ∷ (S Area p u → S Area p u) → Space p u d → Space p u d
-- mapSpaceArea f s | End      ← s = End
--                  | Sarea sa ← s = Sarea (f sa)
--                  | Spc w is ← s = Spc w $ mapSpaceArea f is

-- areaSubtract ∷ Num a ⇒ S Area False a → S Area False a → S Area False a
-- Farea l `areaSubtract` Farea r = Farea $ l ^-^ r

-- sCutOutsideS2 ∷ Di a → Space False a n → Space False a (n + 1)
-- sCutOutsideS2 cut s@Spc{..} = Spc (Fwrap cut cut)                $ s { sWrap = omap (^-^ _diV cut) sWrap }
-- sCutInsideS2  ∷ Fractional a ⇒ Di a → Space False a n → Space False a (n+1)
-- sCutInsideS2  cut s@Spc{..} = Spc (omap (^-^ _diV cut) sWrap) $ s { sWrap = Fwrap cut cut }

-- poRectOppo ∷ Po a → Po a → (Po a, Po a)
-- poRectOppo !(Po (V2 c00 c01)) !(Po (V2 c10 c11))
--   = (Po (V2 c00 c11), Po (V2 c10 c01))

-- poNWSERectArcCentersCW ∷ Num a ⇒ Po a → Po a → R a → (Po a, Po a, Po a, Po a)
-- poNWSERectArcCentersCW !lt@(Po (V2 ltx lty)) !rb@(Po (V2 rbx rby)) (R r) =
--   let (Po (V2 lbx lby), Po (V2 rtx rty)) = poRectOppo lt rb
--   in (Po (V2 (ltx + r) (lty + r))
--      ,Po (V2 (rtx - r) (rty + r))
--      ,Po (V2 (rbx - r) (rby - r))
--      ,Po (V2 (lbx + r) (lby - r)))

-- aRectAnglesNWCW ∷ (Fractional a, Floating a) ⇒ (An2 a, An2 a, An2 a, An2 a)
-- aRectAnglesNWCW
--   = (An2 $ V2 (180 ⋅ degrees) (270 ⋅ degrees)
--     ,An2 $ V2 (-90 ⋅ degrees)   (0 ⋅ degrees)
--     ,An2 $ V2   (0 ⋅ degrees)  (90 ⋅ degrees)
--     ,An2 $ V2  (90 ⋅ degrees) (180 ⋅ degrees))
--   where !degrees = pi/180

-- aRectAngles_MidSWCW ∷ (Fractional a, Floating a) ⇒ (An2 a, An2 a, An2 a, An2 a, An2 a, An2 a)
-- aRectAngles_MidSWCW
--   = (An2 $ V2 (135 ⋅ degrees) (180 ⋅ degrees)
--     ,An2 $ V2 (180 ⋅ degrees) (270 ⋅ degrees)
--     ,An2 $ V2 (-90 ⋅ degrees) (-45 ⋅ degrees)
--     ,An2 $ V2 (-45 ⋅ degrees)   (0 ⋅ degrees)
--     ,An2 $ V2   (0 ⋅ degrees)  (90 ⋅ degrees)
--     ,An2 $ V2  (90 ⋅ degrees) (135 ⋅ degrees))
--   where !degrees = pi/180

-- -- | Narrowing into a positioned 'S Wrap'.
-- stepIntoNW, stepIntoSE ∷ Num a ⇒ S Wrap True a → Po a
-- stepIntoNW (Pwrap (Di pwNWd) _ (Po pwNWp) _) = Po $ pwNWp ^+^ pwNWd
-- stepIntoSE (Pwrap _ (Di pwSEd) _ (Po pwSEp)) = Po $ pwSEp ^-^ pwSEd

-- stepInto ∷ Num a ⇒ S Wrap True a → (Po a, Po a)
-- stepInto pw = (stepIntoNW pw, stepIntoNW pw)

-- -- | Narrowing into a positioned 'Wrap', halfway.
-- stepIntoNW'2, stepIntoSE'2 ∷ Fractional a ⇒ S Wrap True a → Po a
-- stepIntoNW'2 (Pwrap (Di pwNWd) _ (Po pwNWp) _) = Po $ pwNWp ^+^ pwNWd ⋅ 0.5
-- stepIntoSE'2 (Pwrap _ (Di pwSEd) _ (Po pwSEp)) = Po $ pwSEp ^-^ pwSEd ⋅ 0.5

-- stepInto'2 ∷                 Fractional a ⇒ S Wrap True a → (Po a, Po a)
-- stepInto'2 pw = (stepIntoNW'2 pw, stepIntoSE'2 pw)

--- XXX: destroys the depth information
-- sMapInnermost ∷ (Space p d a → Space p e a) → Space p f a → Space p g a
-- sMapInnermost f s
--   | Spc w End ← s = f s
--   | Spc w is  ← s = Spc w $ sMapInnermost f is


-- -- * GMaybe
-- --
-- data GMaybe (t ∷ Bool) a where
--   GNothing ∷ GMaybe False a
--   GJust ∷
--     { gfromJust ∷ a
--     } → GMaybe True a
-- deriving instance Show a ⇒ Show (GMaybe t a)
-- deriving instance Functor (GMaybe t)
-- -- instance Monoid a ⇒ Monoid (GMaybe False a) where
-- --   mempty = GNothing
-- -- instance Monoid a ⇒ Monoid (GMaybe True a)  where
-- --   GJust l `mappend` GJust r = GJust $ l <> r

-- gfromMaybe ∷ a → GMaybe t a → a
-- gfromMaybe def GNothing  = def
-- gfromMaybe _   (GJust x) = x
