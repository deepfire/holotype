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
import           Data.Foldable
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
import qualified Data.Text.Lazy                    as TL
import           Data.Text.Lazy                           (toStrict)
import           Data.Text.IO
import qualified Data.Text.Lazy.IO                 as TLIO
import           Data.Type.Bool
import           Data.Void                                (Void)
import           Text.PrettyPrint.Leijen.Text      hiding ((<>), (<$>), space)

-- Algebra
import           Linear                            hiding (trace, unit)

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

-- * Screen-space dimensions and requirement querying
--
newtype ScreenCstr a = ScreenCstr { _scrcstr ∷ Cstr a } deriving (Eq, Show)
makeLenses ''ScreenCstr

data UnitI
  = UDouble
  | UInt


-- * Requirement
--
data RType where
  RAbsolute  ∷ RType
  RScreenRel ∷ RType
  deriving (Eq, Show)

instance Pretty RType where
  pretty RAbsolute  = text "RAbs"
  pretty RScreenRel = text "RScr"

data Reqmt d where
  Reqmt ∷
    { _rtype ∷ RType
    , _reqt  ∷ Reqt d
    } → Reqmt d
    deriving (Eq, Functor, Show)
makeLenses ''Reqmt

instance Num d ⇒ Monoid (Reqmt d) where
  -- XXX: WARNING: flawed Monoid instance!
  mempty = Reqmt RAbsolute mempty

instance Show d ⇒ Pretty (Reqmt d) where
  pretty (Reqmt ty req) = unreadable (rendCompact $ pretty ty) $ text (ppV2 $ req^.reqt'v)

absolute'reqmt ∷ Num d ⇒ ScreenCstr d → Reqmt d → Reqmt d
absolute'reqmt (ScreenCstr (Cstr scrC)) reqmt@(Reqmt ty (Reqt req)) =
  case ty of
    RAbsolute  → reqmt
    RScreenRel → Reqmt RAbsolute $ Reqt $ req ⋅ scrC

instance Lin d ⇒ AddMax (Reqmt d) where
  addMax ax = Reqmt RAbsolute .: addMax ax `on` _reqt


-- | A sum of minimum and optimum size requirements.
--
data RProduct d where
  RProduct ∷
    { _rp'min  ∷ Reqmt d
    , _rp'opt  ∷ Reqmt d
    } → RProduct d
    deriving (Eq, Functor, Show)
makeLenses ''RProduct

instance Show d ⇒ Pretty (RProduct d) where
  pretty (RProduct min' opt) = unreadable "R" $ pretty min' <+> pretty opt

instance (Lin d, Show d) ⇒ Monoid (RProduct d) where
  mempty = RProduct (Reqmt RAbsolute zero) (Reqmt RAbsolute zero)
  -- | Beware, this is a severely flawed instance.
  --   Sadly, we're dependent on it to query the free applicative.
  l@(RProduct lmin lopt) `mappend` r@(RProduct rmin ropt) =
    if | ropt^.reqt ≡ zero ∧ rmin^.reqt ≡ zero → RProduct lmin lopt
       | lopt^.reqt ≡ zero ∧ lmin^.reqt ≡ zero → RProduct rmin ropt
       | otherwise → errorTL $ "RProduct.⊕ " <> rendCompact (pretty l <+> pretty r)

absolute'rproduct ∷ Num d ⇒ ScreenCstr d → RProduct d → RProduct d
absolute'rproduct scrC r =
  RProduct (absolute'reqmt scrC $ r^.rp'min)
             (absolute'reqmt scrC $ r^.rp'opt)

-- XXX: assumes an absolute product (need to go type-level)
rproduct'δ ∷ Num a ⇒ RProduct a → Reqmt a
rproduct'δ RProduct{..} = Reqmt RAbsolute $ (on (-) _reqt) _rp'opt _rp'min

sum'requirements'axisMajor ∷ (Lin d, Show d) ⇒ Axes → [RProduct d] → RProduct d
sum'requirements'axisMajor axis reqs =
  foldl' (\(RProduct lmin lopt) (RProduct rmin ropt) →
            RProduct (addMax axis lmin rmin) (addMax axis lopt ropt))
  mempty reqs

class Requires a where
  -- | Given a screen constraint contex, return requirements.  We're deliberately
  --   not providing the parent constraint, to enable a single sweeping
  --   requirement computation pass.
  requires ∷ d ~ FixedUnit ⇒ ScreenCstr d → a → RProduct d


-- * Space ~ (Constraint * RProduct * Size * Area)
--
--   Purpose: capturing all possible states in the process of layout computation,
--            culminating in a fully-defined Area in the end.
--
data Space (d ∷ Type) where
  Space ∷
    { _constr  ∷ Maybe (Cstr      d)
    , _require ∷ Maybe (RProduct  d)
    , _size    ∷ Maybe (Size      d)
    , _area    ∷ Maybe (Area'Orig d)
    } → Space d
  deriving (Show)
makeLenses ''Space

instance AreaDict d ⇒ HasArea Space d where
  area'Orig = fromMaybe (error "Asked for an Area of incomplete Space.")
              ∘ (^.Space.area)

empty'space ∷ Space d
empty'space = Space Nothing Nothing Nothing Nothing

prettySpace ∷ (AreaDict d) ⇒ Space d → Doc
prettySpace (Space c r s a) =
        prettyMaybe "*" ((text "Cstr" <:>) ∘ text ∘ ppV2  ∘ (^.cstr'v) <$> c)
    <-> prettyMaybe "*" (prettyR ∘ (^.rp'min) <$> r)
    <-> prettyMaybe "*" (prettyR ∘ (^.rp'opt) <$> r)
    <-> prettyMaybe "*" ((text "Size" <:>) ∘ text ∘ ppV2  ∘ (^.size'v) <$> s)
    <-> prettyMaybe "*" (pretty'Area <$> a)
    <-> prettyMaybe "*" ((text "LU"   <:>) ∘ text ∘ ppV2  ∘ (^.lu'v) ∘ lu <$> a)
    where prettyR (Reqmt ty req) = pretty ty <:> text (ppV2 $ req^.reqt'v)

instance AreaDict d ⇒ Pretty (Space d) where
  pretty = unreadable "Space" ∘ prettySpace

trace'space ∷ AreaDict d ⇒ Space d → Space d
trace'space x = trace (TL.unpack $ ppCompact x) x

mk'reqSpace ∷ RProduct d → Space d
mk'reqSpace x = empty'space & require .~ Just x

-- sp'beside ∷ Space → Orient Card → Space → Space
-- sp'beside (Space _ (RProduct _ _) (Just o)) at what@(Space _ (RProduct _ _ (Just r')) _) =
--   what & orig .~ (Just $ orig'beside at o r r')

sp'inside ∷ Space d → Orient a → Space d → (Space d, Space d)
sp'inside = (⊥)
-- * Questions for type-driven subsetting:
--     1. orientations we can handle
--     2. spaces we can handle

sp'constrained, sp'unconstrained, sp'requiring, sp'sized, sp'complete ∷ AreaDict d ⇒ Space d → Bool
sp'constrained   = (≢ Nothing) ∘ (^.constr)
sp'unconstrained = (≡ Nothing) ∘ (^.constr)
sp'requiring     = (≢ Nothing) ∘ (^.require)
sp'sized         = (≢ Nothing) ∘ (^.size)
sp'complete      = (≢ Nothing) ∘ (^.Space.area)


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



-- * CA: free applicative that contextualises child nodes with:
--   1. Space
--   2. Structure (C)
--
data Kind       = Obj | Align | Constr | Req | Wrap | Grav | HBox | VBox | Grid | FBox
data KArity     = One | Many
data KSize      = SzCons | SzReq | NoSize
data KPosition  = Abs | Rel | NoPos


-- * C & S, functor and structure
--
type CDicts d a = (AreaDict d, Pretty a, Requires a)

data C d a where
  C ∷ CDicts d a ⇒
    { _space  ∷ Space d
    , _struct ∷ S d a
    } → C d a

with'CDicts ∷ (∀ b e. (b ~ a, e ~ d, CDicts e b) ⇒ C e b → c) → C d a → c
with'CDicts f x = x & case x of C _ _ → f

data S d a where
  CObj ∷ CDicts d a ⇒
    { _co      ∷ a
    } → S d a
  CBox ∷ CDicts d a ⇒
    { _caxes   ∷ Axes
    , _cbs     ∷ [Ap (C d) a]
    } → S d a
  CWrap ∷ CDicts d a ⇒
    { _cwNW    ∷ !(Di d) -- ^ The combined offsets _ the left and top sides.
    , _cwSE    ∷ !(Di d) -- ^ The combined offsets _ the right and bottom sides.
    , _cw      ∷ Ap (C d) a
    } → S d a
  -- CRel ∷
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
  deriving ()

instance AreaDict d ⇒ Pretty (C d a) where
  pretty (C sp CObj{..})  = unreadable "Obj"         $ nest 8 $ prettySpace sp
  pretty (C sp CBox{..})  = unreadable
                            (showTL _caxes <> "Box") $ nest 8 $ prettySpace sp <> softline <> (vcat $ pretty <$> _cbs)
  pretty (C sp CWrap{..}) = unreadable "Wrap"        $ nest 8 $ prettySpace sp <> softline <+> pretty _cw

instance AreaDict d ⇒ Pretty (Ap (C d) a) where
  pretty = runAp_ pretty

-- makeLenses ''C

ca'cstr ∷ Num d ⇒ Ap (C d) a → Maybe (Cstr d)
ca'cstr = runAp_ (with'CDicts $ _constr  ∘ _space)
ca'reqt ∷ (Lin d, Show d) ⇒ Ap (C d) a → Maybe (RProduct d)
ca'reqt = runAp_ (_require ∘ _space)
ca'size ∷ Num d ⇒ Ap (C d) a → Maybe (Size d)
ca'size = runAp_ (_size    ∘ _space)
ca'area ∷ AreaDict d ⇒ Ap (C d) a → Maybe (Area'Orig d)
ca'area = runAp_ (_area  ∘ _space)

space    ∷ Lens' (C d a) (Space d)
space    f c               = fmap (\s'  -> c { _space  = s' })  (f $ _space c)
struct   ∷ Lens' (C d a) (S d a)
struct   f c               = fmap (\s'  -> c { _struct = s' })  (f $ _struct c)

children ∷ Lens' (C d a) [Ap (C d) a]
children f c@(C _ s@(CBox _ _))    = fmap (\cs' -> c { _struct = s { _cbs   = cs' } }) (f $ _cbs s)
children _ _ = error "Misapplication of a 'children' lens to a wrong GADT constructor.  Please convince author to go type-level."

child    ∷ Lens' (C d a) (Ap (C d) a)
child    f c@(C _ s@(CWrap _ _ _)) = fmap (\c'  -> c { _struct = s { _cw    = c' } })  (f $ _cw s)
child    _ _ = error "Misapplication of a 'children' lens to a wrong GADT constructor.  Please convince author to go type-level."


-- * Description language
--
-- Note: we're mostly starting un-spaced, where appropriate.
--

lift ∷ (CDicts d a) ⇒ a → Ap (C d) a
lift = liftAp . C empty'space ∘ CObj

hbox, vbox ∷ (CDicts d a) ⇒ [Ap (C d) a] → Ap (C d) a
hbox = liftAp ∘ C empty'space ∘ CBox X
vbox = liftAp ∘ C empty'space ∘ CBox Y

wrap ∷ (CDicts d a) ⇒ Di d → Ap (C d) a → Ap (C d) a
wrap bezel = liftAp ∘ C empty'space ∘ CWrap bezel bezel



-- * Requirement & size computation
--
--  1. assign'reqires, bottom-up: ask leaves about their requirements, in context of a screen size,
--     stepwise combining and propagating this composition upward through internal nodes
--  2. assign'size, top-down: reconcile leaf requirements with screen size constraint
--
assign'requires ∷ (Requires a, d ~ FixedUnit) ⇒ ScreenCstr d → C d a → C d a

assign'requires _ (sp'requiring ∘ _space → True) =
  error "Asked to re-assign requirements to an already-requiring node."
assign'requires scrc c@(C _ (CObj o))     = c & space.require .~ Just (requires scrc o)
assign'requires scrc c@(C _ (CBox ax χs)) =
  let reqd     = hoistAp (with'CDicts $ assign'requires scrc) <$> χs
  in c & children      .~ reqd
       -- The 'fromJust' below should be safe, because we're doing
       -- requirement assignment just above.
       & space.require .~ Just (sum'requirements'axisMajor ax $
                                fromMaybe (error "CBox: unexpected missing child reqt")
                                ∘ ca'reqt <$> reqd)
assign'requires scrc c@(C _ (CWrap nw se χ)) =
  let reqd     = hoistAp (with'CDicts $ assign'requires scrc) χ
  in c & child         .~ reqd
       -- The 'fromJust' below should be safe, because we're doing
       -- requirement assignment just above.
       & space.require .~ Just ((fromMaybe (error "CWrap: unexpected missing child reqt")
                                  $ ca'reqt reqd)
                                & rp'min ∘ reqt ∘ reqt'di %~ (+ (nw + se))
                                & rp'opt ∘ reqt ∘ reqt'di %~ (+ (nw + se)))
assign'requires _ _ = error "assign'requires: missing case"

sp'constraint'changed ∷ AreaDict d ⇒ Cstr d → Space d → Bool
sp'constraint'changed cstr (Space sp'cstr _ _ _) = sp'cstr ≢ Just cstr

assign'size ∷ (Requires a, d ~ FixedUnit) ⇒ ScreenCstr d → Cstr d → C d a → C d a

-- Propagate downward changes
assign'size scrC thisC x@(sp'constraint'changed thisC ∘ _space → True) =
  x & space.constr .~ Just thisC
    & assign'size scrC thisC

assign'size scrC thisC x@(sp'requiring ∘ _space → False) =
  x & assign'requires scrC
    & assign'size     scrC thisC

-- Parent does all the work for objects.
assign'size _ _ x@(C (Space _ _ _ _) (CObj _)) = x

-- | Summary: squeeze children requirements into our constraint,
--   while redistributing the excesses.
--
--   Given a downward constraint, boxes consider it in two contexts:
--   - axis-major, for the axis coincident with the box axis
--   - axis-minor, for the other axis.
--
--   Axis-major size allocation happens through redistribution of slack, where
--   slack is the downward constraint minus the sum of minimum (axis-major)
--   requirements of children.  Computed slack is then redistributed equally
--   between children, until saturation and based on their optimal requirements.
--
--   Axis-minor allocation is the minimum of the axis-minor limit and the ∈'s
--   optimum requirement.  The axis-minor limit is a minumum of:
--   - the downward constraint
--   - maximum of the minimum and optimum (axis-minor) requirements
assign'size scrC thisC o@(C (Space _ _ _ _) (CBox axis _)) =
  let -- Common computations
      chi'allRs         = absolute'rproduct scrC ∘ fromJust ∘ ca'reqt <$> o^.children
      minima            = _reqt ∘ _rp'min <$> chi'allRs
      optima            = _reqt ∘ _rp'opt <$> chi'allRs
      minor'axis        = other'axis axis
      -- Process minor axis first:
      -- 1. find largest (min or max) requirement on the minor axis
      reqs'axis'max reqs = foldl' (addMax axis) mempty reqs ^. reqt'd minor'axis
      minor'maxR        = reqs'axis'max optima `max` reqs'axis'max minima
      -- 2. constrain that with upstream
      minor'alloc       = min minor'maxR $ thisC ^. cstr'd minor'axis
      -- Distribution along the major axis
      step ∷ (Lin d, d ~ FixedUnit) ⇒ Axes → d → d → [(Reqt d, Reqt d)] → [(Reqt d, Reqt d)] → (d, [(Reqt d, Reqt d)])
      step _  _          rem' acc []               = (rem', acc)
      step ax unit'share rem' acc ((now, lack):rs) =
        let accept  = min unit'share (lack ^. reqt'd ax)
            sated'r = (now & reqt'd ax %~ (+ accept), lack & reqt'd ax %~ ((-) accept))
        in step ax unit'share (rem' - accept) (sated'r:acc) rs
      assign'minor'axis ∷ d ~ FixedUnit ⇒ [(Reqt d, Reqt d)] → [(Reqt d, Reqt d)]
      assign'minor'axis pairs =
        [ p & _1 ∘ reqt'd minor'axis .~ min minor'alloc ((sz + δ) ^. reqt'd minor'axis)
        | p@(sz, δ) ← pairs ]
      distribute ∷ (Lin d, Pretty d, Show d, d ~ FixedUnit) ⇒
        Axes → Maybe d → d → [(Reqt d, Reqt d)] → Bool → (d, [(Reqt d, Reqt d)])
      -- Test for convergence (no space left to distribute or nothing lacks it)
      distribute _ (Just last'rem) rem'@((\r→r≡0∨r≡last'rem) → True) rpairs' revved =
        -- every pass of 'step' over 'rpairs' reverses the latter -- keep track of that..
        (,) rem' (rpairs' & (if revved then reverse else id)
                          & assign'minor'axis)
      distribute ax _              rem'                              rpairs' revved =
        let -- Equal (non-proportional) distribution of remainder among children
            unit'share              = rem' / fromIntegral (length rpairs')
            (rem'next, rpairs'next) = step axis unit'share rem' [] rpairs'
        in distribute ax (Just rem') rem'next rpairs'next (not revved)
      -- Process major axis
      lacks             = _reqt ∘ rproduct'δ <$> chi'allRs
      rpairs            = zip minima lacks
      total'lacks       = foldl' (addMax axis) mempty minima
      remainder         = thisC^.cstr'd axis - (total'lacks^.reqt'd axis)
      (,)
       overflow
       (unused, sizes)  = if remainder >= 0
                          then (0,           distribute axis Nothing remainder rpairs False)
                          -- XXX: handle overflow
                          else (-remainder, (0, zip minima lacks))
      -- Assign constraints
      cstrd'sized'chis  = [ hoistAp (with'CDicts
                                     (\χ→ χ & space.constr .~ Just chi'cstr
                                            & space.size   .~ Just (Size sz)
                                            & assign'size scrC chi'cstr)) c
                          | (((Reqt sz), _), c) ← zip sizes (o^.children)
                          , let chi'cstr = Cstr sz ]
  in o & space.size .~ (Just $ Size $ _cstr'di (thisC & cstr'd axis       %~ (flip (-) unused)
                                                      & cstr'd minor'axis .~ minor'alloc))
       & children   .~ cstrd'sized'chis

assign'size scrC thisC o@(C (Space _ _ _ _) (CWrap lu rb χ)) =
  let χC    = thisC & cstr'di %~ (flip (-) (lu + rb))
      χR    = _reqt $ _rp'opt $ absolute'rproduct scrC $ fromJust $ ca'reqt χ
      χDi   = liftA2 min (χC ^. cstr'di) (χR ^. reqt'di)
      sized = hoistAp (with'CDicts (\χ→ χ & space.constr .~ Just (Cstr χDi)
                                          & space.size   .~ Just (Size χDi)
                                          & assign'size scrC (Cstr χDi)))
              χ
  in o & space.size .~ Just (Size $ χDi + lu + rb)
       & child      .~ sized

assign'size _ _ _ = error "assign'size: unhandled case"


-- * Origination
--
-- NOTE: this is based on Origin = Center
-- assign'origins ∷ C d a → C d a
-- * REQUIREMENTS
--
-- For this code we want something more high-level than banging Di's against Po's.
-- This implies, as a first guess:
--  - ability to put things beside each other
--    - Orients
--    - a combination of Di and Po in the combinator

po'add'axisMajor ∷ AreaDict d ⇒ Axes → Di d → Po d → Po d
po'add'axisMajor ax by pos = pos & po'd ax %~ (+ (by ^. (di'd ax)))

assign'origins ∷ AreaDict d ⇒ LU d → C d a → C d a

assign'origins cursor o@(C (Space _ _ (Just sz) _)  CObj{..}) =
  o & space∘Space.area .~ (Just $ Area (lu'orig sz cursor) sz)

assign'origins cursor o@(C (Space _ _ (Just sz) _) (CBox axis chis)) =
  let step ∷ (CDicts d a, AreaDict d) ⇒ LU d → [Ap (C d) a] → [Ap (C d) a] → (LU d, [Ap (C d) a])
      step cur acc []     = (cur, acc)
      step cur acc (x:xs) =
        let x'          = hoistAp (with'CDicts $ assign'origins cur) x
            δ           = ca'size x' ^._Just.size'di
            next'cursor = cur & lu'po %~ po'add'axisMajor axis δ
        in step next'cursor (x':acc) xs
      (_, originated'chis) = step cursor [] chis
  in o & space∘Space.area .~ (Just $ Area (lu'orig sz cursor) sz)
       & children         .~ reverse originated'chis

assign'origins cursor o@(C (Space _ _ (Just sz) _) (CWrap lu rb χ)) =
  let next'cursor = cursor & lu'po %~ po'add (_di'v lu)
  in o & space∘Space.area .~ (Just $ Area (lu'orig sz cursor) sz)
       & child            .~ hoistAp (with'CDicts $ assign'origins next'cursor) χ


-- * Proof-of-existence code
--
instance Requires Char where
  -- XXX: stub
  requires _scrc _d = RProduct (Reqmt RAbsolute $ Reqt $ di 1 1) (Reqmt RAbsolute $ Reqt $ di 2 2)

tree ∷ (d ~ FixedUnit) ⇒ Ap (C d) Char
tree =
  vbox [ lift 'a'
       , wrap (di 1 1) $
         hbox [ lift 'b'
              , lift 'c'
              ]
       , lift 'd'
       ]

tree'canary ∷ (d ~ FixedUnit) ⇒ Ap (C d) Char
tree'canary =
  let cstr  = Cstr $ di 10 10
      orig  = LU $ po 0 0
      reqd  = hoistAp (with'CDicts $ assign'requires (ScreenCstr cstr))  tree
      sized = hoistAp (with'CDicts $ assign'size (ScreenCstr cstr) cstr) reqd
      origd = hoistAp (with'CDicts $ assign'origins orig)                sized
  in origd


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
