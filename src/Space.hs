--{-# OPTIONS_GHC -fplugin GHC.TypeLits.Extra.Solver #-}
--{-# OPTIONS_GHC -fplugin GHC.TypeLits.Normalise #-}
{-# LANGUAGE ConstraintKinds, DataKinds, KindSignatures, TypeApplications, TypeInType #-}
{-# LANGUAGE FlexibleContexts, FlexibleInstances, InstanceSigs, MultiParamTypeClasses, NoMonomorphismRestriction, RankNTypes, TypeSynonymInstances, UndecidableInstances #-}
{-# LANGUAGE GADTs, FunctionalDependencies, TypeFamilies, TypeFamilyDependencies #-}
{-# LANGUAGE BangPatterns, MultiWayIf, OverloadedStrings, RecordWildCards, ScopedTypeVariables, StandaloneDeriving, TypeOperators, UnicodeSyntax, ViewPatterns #-}
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
import           Control.Monad.Random              hiding (lift)
import           Control.Monad.State               hiding (lift)
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
-- ** M, for iMplmenentation requirements
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


-- * Pre-space
--
newtype Cstr d = Cstr { fromCstr ∷ Di d } deriving (Eq, Num, Show)
newtype Reqt d = Reqt { fromReqt ∷ Di d } deriving (Eq, Num, Show)
newtype Orig d = Orig { fromOrig ∷ Po d } deriving (Eq, Num, Show)

instance Num d ⇒ Monoid (Cstr d) where mempty = Cstr $ Di zero
instance Num d ⇒ Monoid (Reqt d) where mempty = Reqt $ Di zero
instance Num d ⇒ Monoid (Orig d) where mempty = Orig $ Po zero

cstr'v f (Cstr (Di v)) = Cstr ∘ Di <$> f v
reqt'v f (Reqt (Di v)) = Reqt ∘ Di <$> f v
orig'v f (Orig (Po v)) = Orig ∘ Po <$> f v

cstr'd X f (Cstr (Di (V2 x y))) = Cstr ∘ Di ∘ (flip V2 y) <$> f x
cstr'd Y f (Cstr (Di (V2 x y))) = Cstr ∘ Di ∘ (id   V2 x) <$> f y
reqt'd X f (Reqt (Di (V2 x y))) = Reqt ∘ Di ∘ (flip V2 y) <$> f x
reqt'd Y f (Reqt (Di (V2 x y))) = Reqt ∘ Di ∘ (id   V2 x) <$> f y
orig'd X f (Orig (Po (V2 x y))) = Orig ∘ Po ∘ (flip V2 y) <$> f x
orig'd Y f (Orig (Po (V2 x y))) = Orig ∘ Po ∘ (id   V2 x) <$> f y

instance Show d ⇒ Pretty (V2   d) where ppL x = format "{}x{}" (showT $ x^._x, showT $ x^._y)
instance Show d ⇒ Pretty (Cstr d) where ppL = format "#<Cstr {}>" ∘ Only ∘ ppL ∘ (^.cstr'v)
instance Show d ⇒ Pretty (Reqt d) where ppL = format "#<Reqt {}>" ∘ Only ∘ ppL ∘ (^.reqt'v)
instance Show d ⇒ Pretty (Orig d) where ppL = format "#<Orig {}>" ∘ Only ∘ ppL ∘ (^.orig'v)

reqt'add  ∷ Lin d ⇒ Axes → Reqt d → Reqt d → Reqt d
reqt'add XY (Reqt (Di (V2 lx ly))) (Reqt (Di (V2 rx ry))) = Reqt ∘ Di $ V2 (lx   +   ly) (rx   +   ry)
reqt'add X  (Reqt (Di (V2 lx ly))) (Reqt (Di (V2 rx ry))) = Reqt ∘ Di $ V2 (lx   +   ly) (rx `max` ry)
reqt'add  Y (Reqt (Di (V2 lx ly))) (Reqt (Di (V2 rx ry))) = Reqt ∘ Di $ V2 (lx `max` ly) (rx   +   ry)

-- * TODO:
-- - alignment as parameter, instead of hard-coded N/W edge alignment
-- - switch to centre-based origin
--
orig'beside ∷ Lin d ⇒ Orient Card → Orig d → Reqt d → Reqt d → Orig d
orig'beside ON o r t = o & orig'v._y %~ ((-)(r^.reqt'v._y))
orig'beside OS o r t = o & orig'v._y %~ ((+)(t^.reqt'v._y))
orig'beside OW o r t = o & orig'v._x %~ ((-)(r^.reqt'v._x))
orig'beside OE o r t = o & orig'v._x %~ ((+)(t^.reqt'v._x))

newtype ScreenC d = ScreenC { _scrc'cstr ∷ Cstr d } deriving (Eq, Show)
makeLenses ''ScreenC

scrc'v ∷ Lens' (ScreenC d) (V2 d)
scrc'v = scrc'cstr ∘  cstr'v
scrc'd = scrc'cstr .: cstr'd


type Constraint d = Maybe (Cstr d)

data Requirement d where
  Requirement ∷
    { _hard ∷ Maybe (Reqt d)
    , _soft ∷ Maybe (Reqt d)
    , _eff  ∷ Maybe (Reqt d)
    } → Requirement d
    deriving (Show)
makeLenses ''Requirement

reqm'has'hard, reqm'has'soft, reqm'has'eff ∷ Requirement d → Bool
reqm'has'hard = isJust ∘ view hard
reqm'has'soft = isJust ∘ view soft
reqm'has'eff  = isJust ∘ view eff

type Origin      d = Maybe (Orig d)

instance Monoid (Requirement d) where
  mempty = Requirement Nothing Nothing Nothing

class Requires a d | a → d where
  hard'requires ∷ ScreenC d → a → Maybe (Reqt d)
  soft'requires ∷             a → Maybe (Reqt d)
  --
  hard'requires _ _ = Nothing
  soft'requires   _ = Nothing

requires ∷ Requires a d ⇒ ScreenC d → a → Requirement d
requires c x = Requirement (hard'requires c x) (soft'requires x) Nothing


-- * Space
--
data Space (d ∷ Type) where
  Space ∷
    { _cstr ∷ Constraint  d
    , _reqt ∷ Requirement d
    , _orig ∷ Origin      d
    } → Space d
  deriving (Show)
makeLenses ''Space

instance Monoid (Space d) where
  mempty = Space Nothing mempty Nothing

instance Show d ⇒ Pretty (Space d) where
  ppL (Space c r o) = format "#<Space {}--{}:{}--{}>"
    ( fromMaybe "*" $ pp ∘ (^.cstr'v) <$> c
    , fromMaybe "*" $ pp ∘ (^.reqt'v) <$> r^.hard
    , fromMaybe "*" $ pp ∘ (^.reqt'v) <$> r^.soft
    , fromMaybe "*" $ pp ∘ (^.orig'v) <$> o)

class IsSpace t d where
  liftSp ∷ t d → Space d


-- * Space constructors

instance IsSpace Cstr d where liftSp x = mempty & cstr .~ Just x
instance IsSpace Reqt d where liftSp x = mempty & reqt .~ Requirement (Just x) Nothing Nothing
instance IsSpace Orig d where liftSp x = mempty & orig .~ Just x

s'beside ∷ Lin d ⇒ Space d → Orient Card → Space d → Space d
s'beside (Space _ (Requirement _ _ (Just r)) (Just o)) at what@(Space _ (Requirement _ _ (Just r')) _) =
  what & orig .~ (Just $ orig'beside at o r r')

s'inside ∷ Space d → Orient a → Space d → (Space d, Space d)
s'inside = (⊥)
-- * Questions for type-driven subsetting:
--     1. orientations we can handle
--     2. spaces we can handle


-- * If we ever need a Place, it's here:
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

dumpC :: (Lin d, Show d) ⇒ Ap (C d) a -> IO a
dumpC m = runAp dump m
  where
    dump :: (Lin d, Show d) ⇒ C d a -> IO a
    dump  CObj{..} = (putStrLn $ "Obj "  <> pp _space) >> pure _cobj
    dump CHBox{..} = (putStrLn $ "HBox " <> pp _space) >> head <$> mapM (runAp dump) _chs

    dump CVBox{..} = (putStrLn $ "VBox " <> pp _space) >> head <$> mapM (runAp dump) _cvs
    dump CWrap{..} = (putStrLn $ "Wrap " <> pp _space) >> runAp dump _cw

-- ca'requires ∷ (Lin d, Requires (C d a) d) ⇒ ScreenC d → Ap (C d) a → Requirement d
-- ca'requires scrc x = runAp_ (requires scrc) x

ca'cstr ∷ (Lin d, Monoid (Constraint d))  ⇒ Ap (C d) a → Constraint d
ca'cstr = runAp_ (^.space.cstr)
ca'reqt ∷ (Lin d, Monoid (Requirement d)) ⇒ Ap (C d) a → Requirement d
ca'reqt = runAp_ (^.space.reqt)
ca'orig ∷ (Lin d, Monoid (Origin d))      ⇒ Ap (C d) a → Origin d
ca'orig = runAp_ (^.space.orig)

ca'set'orig ∷ Ord d ⇒ Origin d → Ap (C d) a → Ap (C d) a
ca'set'orig v = hoistAp (& space.orig .~ v)


-- * Structure
--
-- data CF d a where
--   CF ∷ Show a ⇒
--     { cf'spc    ∷ Space d
--     , cf'obj    ∷ C d k (Ap (C d) a)
--     } → CF d a

space    ∷ Lens' (C d a) (Space d)
space    f c               = fmap (\s'  -> c { _space = s' })  (f $ _space c)

children ∷ Lens' (C d a) [Ap (C d) a]
children f c@(CHBox _ _) = fmap (\cs' -> c { _chs   = cs' }) (f $ _chs c)
children f c@(CVBox _ _) = fmap (\cs' -> c { _cvs   = cs' }) (f $ _cvs c)
children _ _ = error "Misapplication of a 'children' lens to a wrong GADT constructor.  Please convince author to go type-level."

child ∷ Lens' (C d a) (Ap (C d) a)
child f c@(CWrap _ _ _ _)  = fmap (\c'  -> c { _cw    = c' })  (f $ _cw c)
child _ _ = error "Misapplication of a 'children' lens to a wrong GADT constructor.  Please convince author to go type-level."

data    C d a where
  CObj ∷
    { _space   ∷ Space d
    , _cobj    ∷ a
    } → C d a
  CHBox ∷
    { _space   ∷ Space d
    , _chs     ∷ [Ap (C d) a]
    } → C d a
  CVBox ∷
    { _space   ∷ Space d
    , _cvs     ∷ [Ap (C d) a]
    } → C d a
  CWrap ∷
    { _space   ∷ Space d
    , _cwNW    ∷ !(Di d) -- ^ The combined offsets _ the left and top sides.
    , _cwSE    ∷ !(Di d) -- ^ The combined offsets _ the right and bottom sides.
    , _cw      ∷ Ap (C d) a
    } → C d a
    deriving (Functor, Generic)
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
lift ∷ Ord d ⇒ Reqt d → a → Ap (C d) a
lift = liftAp .: CObj ∘ liftSp

hbox, vbox ∷ Ord d ⇒ [Ap (C d) a] → Ap (C d) a
hbox = liftAp ∘ CHBox mempty
vbox = liftAp ∘ CVBox mempty


hb ∷ Lin a ⇒ Ap (C Double) a
hb =
  hbox
  [ lift (Reqt $ di 10 10) 0
  , lift (Reqt $ di 10 10) 1
  ]

demo ∷ Lin a ⇒ Ap (C Double) a
demo =
  vbox
  [ lift (Reqt $ di 10 10) 0
  , hb
  ]



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
assign'eff'requires ∷ (Lin d, Show d, Requires (C d a) d, Requires a d) ⇒ ScreenC d → C d a → C d a

-- If we already have an effective requirement, there's nothing to do
assign'eff'requires _ x@(isJust ∘ (^. space.reqt.hard) → True) = x

assign'eff'requires _      (CObj  (Space  Nothing  _                        _) _) = error "Asked to assign requires to an object without constraints."
assign'eff'requires scrc x@(CObj  (Space (Just _) (Requirement _ _ Nothing) _) o) = x & space.reqt .~ requires scrc o

assign'eff'requires scrc x@(CHBox (Space (Just this'cstr) _ _) _) =
  let -- Query soft/hard requirements
      reqs             = ca'reqt <$> x^.children
      partition'by'hardness xs acc@(hs, ss)
        | []      ← xs = acc
        | (x:xs') ← xs = partition'by'hardness xs'
                         $ case x of
                             Requirement (Just h) _ _ → (h:hs, ss)
                             Requirement _ (Just s) _ → (hs, s:ss)
                             _                        → (hs, mempty:ss) -- XXX: interprets no requirement at all as zero requirement
      (,) hards softs  = partition'by'hardness reqs ([], [])
      hard'sum         = foldr (reqt'add X) mempty hards                -- XXX: softs are ignored wrt. the secondary axes
      soft'abs'remains = this'cstr^.cstr'd X - hard'sum^.reqt'd X
      soft'abs'ratio   = soft'abs'remains / (sum $ (^.reqt'd X) <$> softs)
      soft'to'abs      = (⋅ soft'abs'ratio)
      req'cstr (Requirement (Just h) _        _) = Cstr $ Di $ h^.reqt'v
      req'cstr (Requirement _        (Just s) _) = Cstr $ Di $ s^.reqt'v
      -- Assign effective requirements
      chis             = [ hoistAp (& space.cstr._Just .~ req'cstr req) c
                         | (req, c) ← zip reqs (x^.children) ]
  in x & space.reqt.eff._Just.reqt'v .~ this'cstr^.cstr'v
       & children       .~ chis

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

space'op ∷ Space d → [C d a] → (C d a → State (Space d) (C d a)) → ([C d a], Space d)
space'op spc xs f = runState (mapM f xs) spc

cursor'state'op ∷ Po d → [C d a] → (C d a → State (Po d) (C d a)) → ([C d a], Po d)
cursor'state'op y xs f = runState (mapM f xs) y


-- * Origination
--
-- NOTE: this is based on Origin = Center
assign'origins ∷ (Lin d, Show d) ⇒ C d a → C d a
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
