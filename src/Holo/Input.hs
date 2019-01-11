{-# OPTIONS_GHC -Wall #-}
{-# OPTIONS_GHC -Wno-unticked-promoted-constructors -Wno-implicit-prelude -Wno-monomorphism-restriction -Wno-name-shadowing -Wno-all-missed-specialisations -Wno-unsafe -Wno-missing-export-lists -Wno-type-defaults #-}
module Holo.Input
  (
  --
    Sem, SemW, mkSem
  , SemDescKey, SemLongText
  , SemSubs
  --
  , SemStore
  , declSemStore, lookupSemDesc, lookupSemId
  --
  , SemListeners, semListen
  --
  , AElt(..)
  --
  , ListenerBinds(..), LBinds, lbsAE, lbsSubs
  , listenerBindsParse, descBindsQuery, childBinds, childSubs
  --
  , EvBinds, bindSem
  --
  , EvK(..), EvTy(..)
  , Ev(..), evTy
  , Ev'(..), ev'Ty
  , promoteEv
  --
  , EvMask
  , evMatch, evMaskTypes
  --
  , Input, mkInput
  , inStore, inBinds, inMux
  , EvMux(..)
  --
  , Subscription
  , subSingleton, subsByType, resolveSubs
  --
  , routeEv
  --
  , glfwMask
  , inputMaskKeys, inputMaskKey, inputMaskKeyPress, inputMaskKeyPress', inputMaskKeyRelease
  , inputMaskChars, inputMaskButtons, inputMaskClick1Press, inputMaskClick1Release, inputMaskClick1Any, editMaskKeys
  )
where

import           Control.Arrow                            ((***))
import           Control.Lens                             ((<&>))
import           Control.Monad.IO.Class                   (MonadIO, liftIO)
-- import           Control.Monad                            (foldM)
import           Data.Functor.Misc                        (Const2(..))
import           Data.Foldable                            (toList, foldr')
import           Data.Function                            (on)
import           Data.Maybe                               (fromMaybe)
import qualified Data.List                         as L
import qualified Data.Text                         as T
import           Data.String                              (IsString(..))
import qualified Data.IntUnique                    as U
import           Prelude.Unicode
import           Reflex
import           Text.Printf                              (printf)
import "GLFW-b"  Graphics.UI.GLFW                  as GL
import qualified Data.IntMap.Strict                as IntMap
import qualified Data.Map.Monoidal.Strict          as MMap
import qualified Data.Map.Strict                   as Map
import qualified Data.Sequence                     as Seq
import qualified Data.Set                          as Set
import           Debug.Trace                              (trace)
import qualified Reflex.GLFW                       as GLFW
import           Reflex.GLFW                              (RGLFW, InputU(..))
-- import qualified Data.IORef                        as IO
-- import qualified System.IO.Unsafe                  as IO

import           Graphics.Flatland
import           Holo.Prelude
import           Holo.Port                                (IdToken, tokenHash)
import qualified Holo.Port                         as Port


-- | Sem: a semantic event.
--
data Sem a where
  Sem ∷
    { semId       ∷ !U.Unique
    , semDescKey  ∷ !SemDescKey
    , semLongText ∷ !SemLongText -- XXX
    } → Sem a
    deriving Eq

newtype SemDescKey  = SemDescKey  { _sdkVal ∷ T.Text } deriving (Eq, IsString, Ord)
newtype SemLongText = SemLongText { _sltVal ∷ T.Text } deriving (Eq, IsString)

data SemW = ∀ a. SemW { _fromSemW ∷ Sem a }

-- semWId ∷ SemW → U.Unique
-- semWId (SemW (Sem id _ _))= id

instance Eq SemW where
  SemW (Sem idA _ _) == SemW (Sem idB _ _) = idA ≡ idB

instance Ord SemW where
  SemW (Sem idA _ _) `compare` SemW (Sem idB _ _) = compare idA idB

instance Show SemW where
  show (SemW (Sem i (SemDescKey sdk) _)) = printf "(Sem %d \"%s\")" (U.hashUnique i) (T.unpack sdk)

mkSem ∷ (MonadIO m) ⇒ SemDescKey → SemLongText → m SemW
mkSem semDescKey semLongText = liftIO $ do
  semId ← U.newUnique
  pure $ SemW $ Sem{..}

newtype SemSubs = SemSubs (Set.Set SemW) deriving (Semigroup, Monoid)

instance Show SemSubs where
  show (SemSubs ss) = printf "(SemSubs %s)" (L.intercalate " " $ (\(SemW Sem{..})→ T.unpack $ _sdkVal semDescKey) <$> Set.toList ss)


-- | SemStore:  globally recognised pool of SemDeskKey-keyed Sems.
--
newtype SemStoreName = SemStoreName T.Text deriving (IsString)

newtype SemStore = SemStore (SemStoreName, IntMap.IntMap SemW, Map.Map SemDescKey SemW)

instance Semigroup SemStore where
  SemStore (n, li, ld) <> SemStore (_, ri, rd) =
    SemStore (n, li <> ri, ld <> rd)

instance Monoid SemStore where
  mempty = SemStore ("", mempty, mempty)

semStoreAdd ∷ SemW → SemStore → SemStore
semStoreAdd s@(SemW (Sem sid sdk _)) (SemStore (name, im, dm)) = SemStore
  ( name
  , IntMap.insert (U.hashUnique sid) s im
  , Map.insert    sdk                s dm)

declSemStore ∷ ∀ m. MonadIO m ⇒ SemStoreName → [(SemDescKey, SemLongText)] → m SemStore
declSemStore name = (foldr' semStoreAdd (SemStore (name, mempty, mempty)) <$>) ∘ traverse (uncurry mkSem)

lookupSemDesc ∷ SemStore → SemDescKey → Maybe SemW
lookupSemDesc (SemStore (_,_,dm)) = flip Map.lookup dm

lookupSemId   ∷ SemStore → U.Unique   → Maybe SemW
lookupSemId   (SemStore (_,im,_)) = flip IntMap.lookup im ∘ U.hashUnique

semByDesc ∷ SemStore → SemDescKey → SemW
semByDesc s@(SemStore (SemStoreName name,_,_)) sdk@(SemDescKey k) = flip fromMaybe (lookupSemDesc s sdk) $
  error $ printf "SemStore '%s' doesn't have DescKey '%s'." name k

_semById   ∷ SemStore → U.Unique   → SemW
_semById s@(SemStore (SemStoreName name,_,_)) uid = flip fromMaybe (lookupSemId s uid) $
  error $ printf "SemStore '%s' doesn't have Id '%d'." name (U.hashUnique uid)


-- | SemListeners: what IdTokens listen to particular Sems.
--
newtype SemListeners = SemListeners (IntMap.IntMap (Set.Set IdToken))

instance Semigroup SemListeners where
  SemListeners l <> SemListeners r =
    SemListeners $ IntMap.unionWith Set.union l r

deriving instance Monoid SemListeners

semListen ∷ SemListeners → SemW → IdToken → SemListeners
semListen (SemListeners im) (SemW (Sem sid _ _)) idt = SemListeners $
  IntMap.alter (Just ∘ \case
                   Nothing   → Set.singleton idt
                   Just idts → Set.insert idt idts)
  (U.hashUnique sid) im


-- | AElt/AFull: listener naming hierarchy.
--
newtype AElt  = AElt  { aeltName  ∷ T.Text } deriving (Eq, IsString, Ord)
newtype AFull = AFull               T.Text   deriving (IsString)

aeltParse ∷ AFull → [AElt]
aeltParse (AFull s) = AElt <$> T.splitOn "." s


-- | Listener hierarchy, with names bound to sets of Sems being listened to.
--
--   Specifically, the entity with name ending with AElt:
--   1. Handles SemW messages.
--   2. Has further namespace refinement.
newtype ListenerBinds = LBinds ( AElt
                               , SemSubs
                               , Map.Map AElt ListenerBinds
                               )
instance Show ListenerBinds where
  show (LBinds (ae, subs, cs)) = printf "(LBinds %s: %s  %s)"
    (T.unpack $ aeltName ae) (show subs) (L.intercalate " " $ show <$> Map.elems cs)

instance IsString LBinds where
  fromString x = LBinds (AElt $ T.pack x, mempty, mempty)

instance Semigroup ListenerBinds where
  LBinds (sA, (SemSubs semsA), chiA) <> LBinds (sB, (SemSubs semsB), chiB)
    = if sA ≢ sB then error $ printf "Asked to merge incompat ListenerBinds with top names '%s' and '%s'." (aeltName sA) (aeltName sB)
      else LBinds (sA, SemSubs $ semsA <> semsB, Map.unionWith (<>) chiA chiB)

type LBinds = ListenerBinds

lbsAE   ∷ ListenerBinds → AElt
lbsAE   (LBinds (x, _, _)) = x

lbsSubs ∷ ListenerBinds → SemSubs
lbsSubs (LBinds (_, x, _)) = x

emptyLBinds ∷ AElt → ListenerBinds
emptyLBinds = LBinds ∘ (,mempty,mempty)

listenerBindsParse ∷ AElt → SemStore → [(AFull, SemDescKey)] → ListenerBinds
listenerBindsParse self store bdescs =
  let binds ∷ AElt → [([AElt], SemDescKey)] → ListenerBinds
      binds ae spls =
        let this, subs ∷ [([AElt], SemDescKey)]
            (this, subs) = L.partition (null ∘ fst) spls
        in LBinds
           ( ae
           , SemSubs $ Set.fromList (semByDesc store ∘ snd <$> this)
           , let grps = L.groupBy ((≡) `on` head ∘ fst) subs
             in Map.fromList $ flip fmap grps
                \xs@((sae:_,_):_)→
                  (sae, binds sae ((tail *** id) <$> xs)))
      res = binds self [ (aeltParse af, sdk) | (af, sdk) ← bdescs]
  in trace (printf "Parsed binds: %s" (show res)) $
     res

descBindsQuery ∷ String → AElt → ListenerBinds → String
descBindsQuery desc ae (LBinds (pae, _, aem)) =
  printf "%s: %s → %s\n%s → %s"
         desc (T.unpack $ aeltName pae) (T.unpack $ aeltName ae)
         (L.intercalate " " $ T.unpack ∘ aeltName <$> Map.keys aem) (show $ Map.lookup ae aem)

childBinds ∷ AElt → ListenerBinds → ListenerBinds
childBinds ae (LBinds (_, _, aem)) =
  -- trace (descBindsQuery "childBinds" ae lbs) $
  fromMaybe (emptyLBinds ae) $ Map.lookup ae aem

childSubs ∷ AElt → ListenerBinds → SemSubs
childSubs = lbsSubs .: childBinds


-- | EvBinds:  resolving Sems to low-level input events.
--
newtype EvBinds = EvBinds (IntMap.IntMap EvMask)

instance Semigroup EvBinds where
  EvBinds l <> EvBinds r =
    EvBinds $ l <> r

deriving instance Monoid EvBinds

bindSem' ∷ SemW → EvMask → EvBinds → EvBinds
bindSem' (SemW (Sem sid _ _)) mask (EvBinds im) = EvBinds $
  IntMap.alter (Just ∘ \case
                   Nothing       → mask
                   Just _premask → mask)
  (U.hashUnique sid) im

bindSem ∷ SemStore → SemDescKey → EvMask → EvBinds → EvBinds
bindSem st k v = bindSem' (semByDesc st k) v

lookupSem ∷ EvBinds → SemW → Maybe EvMask
lookupSem (EvBinds im) (SemW (Sem sid _ _)) =
  IntMap.lookup (U.hashUnique sid) im

translateSem ∷ EvBinds → SemW → EvMask
translateSem binds sem@(SemW (Sem _ (SemDescKey sdk) _)) = flip fromMaybe (lookupSem binds sem) $
  error $ printf "Cannot translate Sem '%s'." sdk

-- XXX: global state
-- semStore ∷ IO.IORef SemStore
-- semStore = IO.unsafePerformIO (IO.newIORef $ SemStore mempty)

-- newSem ∷ (MonadIO m) ⇒ T.Text → m SemW
-- newSem desc = liftIO do
--   sem ← mkSem desc
--   IO.modifyIORef semStore (semStoreAdd sem)
--   pure sem


-- * Ev
--
data EvK
  = GLFWEvK
  | ClickEvK
  | WinSizeEvK
  deriving (Eq, Ord, Show)

data EvTy
  = GLFWEvTy GLFW.EventType
  | ClickEvTy
  | WinSizeEvTy
  deriving (Eq, Ord, Show)

data Ev' (k ∷ EvK) where
  GLFWEv ∷
    { geGLFW             ∷ !GLFW.InputU
    } → Ev' GLFWEvK
  ClickEv ∷
    { ceEv               ∷ !(GLFW.Input GLFW.MouseButton)
    , ceIdToken          ∷ !IdToken
    } → Ev' ClickEvK
  WinSizeEv ∷
    { wsNewSize          ∷ !(Port.ScreenDim (Di Int))
    } → Ev' ClickEvK

instance Show (Ev' k) where
  show (GLFWEv e)      = "#<GLFWEv "    <> show e                    <> ">"
  show (ClickEv e tok) = "#<ClickEv "   <> show e <> " " <> show tok <> ">"
  show (WinSizeEv sz)  = "#<WinSizeEv " <> show sz                   <> ">"

data Ev where
  Ev ∷ { ev ∷ !(Ev' k) } → Ev

ev'Ty ∷ Ev' k → EvTy
ev'Ty = \case
  GLFWEv (GLFW.U x) → GLFWEvTy $ GLFW.eventType x
  ClickEv _ _       → ClickEvTy
  WinSizeEv _       → WinSizeEvTy

evTy ∷ Ev → EvTy
evTy (Ev x) = ev'Ty x


-- * EvMask
--
data EvMask where
  EvMask ∷
    { emGLFW    ∷ !GLFW.EventMask
    , emClick   ∷ !Bool
    , emWinSize ∷ !Bool
    } → EvMask
  deriving (Eq, Ord)

instance Show      EvMask where show EvMask{..} = printf "(IM %s click:%s winsize:%s)" (show emGLFW) (show emClick)
instance Semigroup EvMask where EvMask ga ca wa <> EvMask gb cb wb = EvMask (ga <> gb) (ca ∨ cb) (wa ∨ wb)
instance Monoid    EvMask where mempty = EvMask mempty False False

evMatch ∷ EvMask → Ev → Bool
evMatch EvMask{..} (Ev e) = case e of
  GLFWEv{geGLFW=GLFW.U x} → GLFW.eventMatch emGLFW x
  ClickEv{}               → emClick
  WinSizeEv{}             → emWinSize

evMaskTypes ∷ EvMask → [EvTy]
evMaskTypes EvMask{..} =
  (GLFWEvTy <$> GLFW.eventMaskTypes emGLFW)
  <> [ ClickEvTy   | emClick ]
  <> [ WinSizeEvTy | emWinSize ]

promoteEv ∷ MonadIO m ⇒ GLFW.InputU → m Ev
promoteEv = \case
  U (GLFW.EventFramebufferSize _ w h)
    → pure ∘ Ev ∘ WinSizeEv ∘ Port.ScreenDim $ unsafe'di w h
  x → pure $ Ev $ GLFWEv x


-- * The Input context
--
newtype EvMux t = EvMux { fromEvMux ∷ (EventSelector t (Const2 IdToken Ev)) }

newtype Input t = Input (SemStore, Dynamic t EvBinds, EvMux t)

mkInput ∷ SemStore → Dynamic t EvBinds → EvMux t → Input t
mkInput x y z = Input (x, y, z)

inStore ∷ Input t → SemStore
inStore (Input (x, _, _)) = x

inBinds ∷ Input t → Dynamic t EvBinds
inBinds (Input (_, x, _)) = x

inMux   ∷ Input t → EvMux t
inMux   (Input (_, _, x)) = x


-- * Subscription of IdTokens to EvMasks
--
newtype Subscription = Subscription (MMap.MonoidalMap EvTy (Seq.Seq (IdToken, EvMask)))

instance Show Subscription where
  show (Subscription map) = ("(Subs"<>) ∘ (<>")") $ concat $
    [ " "<>show et<>"::"<> L.intercalate "+" [ printf "0x%x:%s:cl=%s:ws=%s" tok (show gm) (show cl) (show ws)
                                             | (tokenHash → tok,(EvMask gm cl ws)) ← toList subs]
    | (et, subs) ← MMap.toList map]

instance Semigroup Subscription where
  Subscription a <> Subscription b = Subscription $ a <> b

deriving newtype instance Monoid Subscription

subSingleton ∷ IdToken → EvMask → Subscription
subSingleton tok im = Subscription $
  MMap.fromList [ (evty, Seq.singleton (tok, im))
                | evty ← evMaskTypes im ]

subsByType ∷ Subscription → EvTy → Maybe (Seq.Seq (IdToken, EvMask))
subsByType (Subscription ss) ty = MMap.lookup ty ss

resolveSubs ∷ Reflex t ⇒ Input t → IdToken → SemSubs → Dynamic t Subscription
resolveSubs input tok (SemSubs ss) = inBinds input <&>
  (\binds→
     let subs = L.foldl' (<>) mempty $ subSingleton tok ∘ translateSem binds <$> Set.toList ss
     in trace (printf "tok %d ss: %s ⇒ subs: %s\n" (tokenHash tok) (show ss) (show subs)) subs)


routeEv ∷ ∀ t m. (RGLFW t m)
           ⇒ Event t Ev              -- ^ Events to distribute
           → Event t (Ev' ClickEvK)  -- ^ Carries the (possibly) new picked entity
           → Dynamic t Subscription  -- ^ The total mass of subscriptions
           → m (EvMux t)             -- ^ Global event wire for all IdTokens
routeEv evE clickedE subsD = do
  pickeD ← holdDyn Nothing $ Just ∘ ceIdToken <$> clickedE -- Compute the latest focus (or just a mouse click)
  -- XXX: filter the above on the left click -- as it stands any mouse button changes selection
  let fullInputE = leftmost [Ev <$> clickedE, evE]
      inputsD = zipDynWith (,) pickeD (traceDyn "===== new subs: " subsD)
      -- | Process the incoming events using the latest listener and total set subscriptions
      routedE ∷ Event t (Map.Map IdToken Ev)
      routedE = routeSingle <$> attachPromptlyDyn inputsD fullInputE
      routeSingle ∷ ((Maybe IdToken, Subscription), Ev) → Map.Map IdToken Ev
      routeSingle ((mClickOrPicked, subs), ev) =
        let eventType = evTy ev
        in case subsByType subs eventType of
           Nothing         → trace ("ignored:" <> show eventType) mempty -- no-one cares about this type of events
           Just eventTypeSubscribers  →
             let eventListenerSet ∷ Seq.Seq (IdToken, EvMask) =
                   flip Seq.filter eventTypeSubscribers (flip evMatch ev ∘ snd)
             in case (eventType, trace ("routing: "<>show mClickOrPicked) mClickOrPicked, toList eventListenerSet) of
                  (_, _, []) → mempty -- no-one's event mask matches this event
                  -- --------------- here we need the click, not the accumulated pick
                  -- (GLFW.MouseButton, Just clicked, eventListeners) → case lookup clicked eventListeners of
                  --   Nothing → mempty -- focused ID is not among subscribers
                  --   Just x  → if x ≡ clicked
                  --             then M.singleton clicked ev
                  --             else mempty
                  (_, Just pick, eventListeners)  → case lookup pick eventListeners of
                    Nothing → mempty -- focused ID is not among subscribers
                    Just _  → Map.singleton (trace ("routed to: "<>show pick) pick) ev
                  -- (_, Nothing, (tok, _):_) → M.singleton tok ev
                  (_, Nothing, _) → mempty
  pure ∘ EvMux $ fanMap routedE


-- * Various event masks
--

glfwMask ∷ GLFW.EventMask → EvMask
glfwMask x = EvMask x False False

inputMaskKeys    ∷ Set.Set GL.Key → Set.Set GL.KeyState → GL.ModifierKeys → EvMask
inputMaskKeys ks kss mks = glfwMask $ GLFW.eventMaskKeys $ GLFW.KeyEventMask ks kss mks

inputMaskKey     ∷ GL.Key → Set.Set GL.KeyState → GL.ModifierKeys → EvMask
inputMaskKey k kss mks = glfwMask $ GLFW.eventMaskKeys $ GLFW.KeyEventMask (Set.singleton k) kss mks

inputMaskKeyPress ∷ GL.Key → GL.ModifierKeys → EvMask
inputMaskKeyPress k mks = glfwMask $ GLFW.eventMaskKeys $ GLFW.KeyEventMask (Set.singleton k) (Set.singleton GL.KeyState'Pressed) mks

inputMaskKeyPress' ∷ GL.Key → EvMask
inputMaskKeyPress' k = glfwMask $ GLFW.eventMaskKeys $ GLFW.KeyEventMask (Set.singleton k) (Set.singleton GL.KeyState'Pressed) mempty

inputMaskKeyRelease ∷ GL.Key → GL.ModifierKeys → EvMask
inputMaskKeyRelease k mks = glfwMask $ GLFW.eventMaskKeys $ GLFW.KeyEventMask (Set.singleton k) (Set.singleton GL.KeyState'Released) mks

inputMaskChars   ∷ EvMask
inputMaskChars   = glfwMask $ GLFW.eventMaskChars

inputMaskButtons ∷ GLFW.ButtonEventMask → EvMask
inputMaskButtons = glfwMask ∘ GLFW.eventMaskButtons

inputMaskClick ∷ GL.MouseButton → GL.MouseButtonState → EvMask
inputMaskClick btn state = glfwMask $ GLFW.eventMaskButtons $ GLFW.ButtonEventMask (Set.singleton btn) (Set.singleton state) mempty

inputMaskClick1Press, inputMaskClick1Release ∷ EvMask
inputMaskClick1Press   = inputMaskClick GL.MouseButton'1 GL.MouseButtonState'Pressed
inputMaskClick1Release = inputMaskClick GL.MouseButton'1 GL.MouseButtonState'Released

inputMaskClick1Any ∷ EvMask
inputMaskClick1Any = glfwMask $ GLFW.eventMaskButtons $ GLFW.ButtonEventMask (Set.singleton GL.MouseButton'1) (Set.fromList [GL.MouseButtonState'Pressed, GL.MouseButtonState'Released]) mempty

editMaskKeys ∷ EvMask
editMaskKeys = (inputMaskChars <>) $ glfwMask $ GLFW.eventMaskKeys $ GLFW.KeyEventMask
  (Set.fromList
   [ GL.Key'Up, GL.Key'Down, GL.Key'Left, GL.Key'Right, GL.Key'Home, GL.Key'End
   , GL.Key'Backspace, GL.Key'Delete, GL.Key'Enter
   ])
  (Set.fromList [GL.KeyState'Pressed, GL.KeyState'Repeating])
  (mempty)
