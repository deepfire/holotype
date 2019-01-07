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
  , ListenerBinds(..), LBinds, lbsAE
  , listenerBindsParse, childBinds
  --
  , EventBinds
  , bindEvent, lookupEvent, translateEvent
  --
  , InputEvent(..)
  , inputEventType
  , InputEventMask
  , inputMatch
  , inputMaskKeys, inputMaskKey, inputMaskKeyPress, inputMaskKeyRelease
  , inputMaskChars, inputMaskButtons, inputMaskClick1Press, inputMaskClick1Release, editMaskKeys
  --
  , InputEventMux
  , Input, mkInput
  , inStore, inBinds, inMux
  --
  , Subscription
  , subSingleton, subsByType, resolveSubs
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
import qualified Reflex.GLFW                       as GLFW
-- import qualified Data.IORef                        as IO
-- import qualified System.IO.Unsafe                  as IO

import           Holo.Port                                (IdToken, tokenHash)


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

semWId ∷ SemW → U.Unique
semWId (SemW (Sem id _ _))= id

instance Eq SemW where
  SemW (Sem idA _ _) == SemW (Sem idB _ _) = idA ≡ idB

instance Ord SemW where
  SemW (Sem idA _ _) `compare` SemW (Sem idB _ _) = compare idA idB

mkSem ∷ (MonadIO m) ⇒ SemDescKey → SemLongText → m SemW
mkSem semDescKey semLongText = liftIO $ do
  semId ← U.newUnique
  pure $ SemW $ Sem{..}

newtype SemSubs = SemSubs (Set.Set SemW) deriving (Semigroup, Monoid)


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

semById   ∷ SemStore → U.Unique   → SemW
semById s@(SemStore (SemStoreName name,_,_)) uid = flip fromMaybe (lookupSemId s uid) $
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
newtype AFull = AFull { afullName ∷ T.Text } deriving (IsString)

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

instance IsString LBinds where
  fromString x = LBinds (AElt $ T.pack x, mempty, mempty)

instance Semigroup ListenerBinds where
  LBinds (sA, (SemSubs semsA), chiA) <> LBinds (sB, (SemSubs semsB), chiB)
    = if sA ≢ sB then error $ printf "Asked to merge incompat ListenerBinds with top names '%s' and '%s'." (aeltName sA) (aeltName sB)
      else LBinds (sA, SemSubs $ semsA <> semsB, Map.unionWith (<>) chiA chiB)

type LBinds = ListenerBinds

lbsAE ∷ ListenerBinds → AElt
lbsAE (LBinds (x, _, _)) = x

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
  in binds self [ (aeltParse af, sdk) | (af, sdk) ← bdescs]

childBinds ∷ ListenerBinds → AElt → ListenerBinds
childBinds (LBinds (_, _, aem)) ae = fromMaybe (emptyLBinds ae) $ flip Map.lookup aem ae


-- | EventBinds:  resolving Sems to low-level input events.
--
newtype EventBinds = EventBinds (IntMap.IntMap InputEventMask)

instance Semigroup EventBinds where
  EventBinds l <> EventBinds r =
    EventBinds $ l <> r

deriving instance Monoid EventBinds

bindEvent' ∷ SemW → InputEventMask → EventBinds → EventBinds
bindEvent' (SemW (Sem sid _ _)) mask (EventBinds im) = EventBinds $
  IntMap.alter (Just ∘ \case
                   Nothing      → mask
                   Just premask → mask)
  (U.hashUnique sid) im

bindEvent ∷ SemStore → SemDescKey → InputEventMask → EventBinds → EventBinds
bindEvent st k v = bindEvent' (semByDesc st k) v

lookupEvent ∷ EventBinds → SemW → Maybe InputEventMask
lookupEvent (EventBinds im) (SemW (Sem sid _ _)) =
  IntMap.lookup (U.hashUnique sid) im

translateEvent ∷ EventBinds → SemW → InputEventMask
translateEvent binds sem@(SemW (Sem _ (SemDescKey sdk) _)) = flip fromMaybe (lookupEvent binds sem) $
  error $ printf "Cannot translate Sem '%s'." sdk

-- XXX: global state
-- semStore ∷ IO.IORef SemStore
-- semStore = IO.unsafePerformIO (IO.newIORef $ SemStore mempty)

-- newSem ∷ (MonadIO m) ⇒ T.Text → m SemW
-- newSem desc = liftIO do
--   sem ← mkSem desc
--   IO.modifyIORef semStore (semStoreAdd sem)
--   pure sem


-- * InputEvent
--
data InputEvent where
  InputEvent ∷
    { inInputEvent       ∷ GLFW.InputU
    } → InputEvent
  ClickEvent ∷
    { inMouseButtonEvent ∷ GLFW.Input GLFW.MouseButton
    , inIdToken          ∷ IdToken
    } → InputEvent
  deriving (Show)

inputEventType ∷ InputEvent → GLFW.EventType
inputEventType = \case
  InputEvent{inInputEvent=GLFW.U x} → GLFW.eventType x
  ClickEvent{inMouseButtonEvent=_x} → GLFW.MouseButton


-- * InputEventMux
--
type InputEventMux t     = EventSelector t (Const2 IdToken InputEvent)


-- * The Input context
--
newtype Input t = Input (SemStore, Dynamic t EventBinds, InputEventMux t)

mkInput ∷ SemStore → Dynamic t EventBinds → InputEventMux t → Input t
mkInput x y z = Input (x, y, z)

inStore ∷ Input t → SemStore
inStore (Input (x, _, _)) = x

inBinds ∷ Input t → Dynamic t EventBinds
inBinds (Input (_, x, _)) = x

inMux   ∷ Input t → InputEventMux t
inMux   (Input (_, _, x)) = x


-- * InputEventMask
--
data InputEventMask where
  InputEventMask ∷
    { inputMask ∷ GLFW.EventMask
    } → InputEventMask
  deriving (Eq, Ord)
instance Show InputEventMask where
  show InputEventMask{..} = ("(IM "<>) ∘ (<>")") $ show inputMask
instance Semigroup InputEventMask where
  InputEventMask a <> InputEventMask b = InputEventMask $ a <> b
instance Monoid InputEventMask where
  mempty = InputEventMask mempty

inputMatch ∷ InputEventMask → InputEvent → Bool
inputMatch InputEventMask{..} = \case
  InputEvent{inInputEvent=GLFW.U x} → GLFW.eventMatch inputMask x
  ClickEvent{inMouseButtonEvent=x}  → GLFW.eventMatch inputMask x

inputMaskKeys    ∷ Set.Set GL.Key → Set.Set GL.KeyState → GL.ModifierKeys → InputEventMask
-- inputMaskKeys = InputEventMask ∘ GLFW.eventMaskKeys .:: GLFW.KeyEventMask
inputMaskKeys ks kss mks = InputEventMask $ GLFW.eventMaskKeys $ GLFW.KeyEventMask ks kss mks

inputMaskKey     ∷ GL.Key → Set.Set GL.KeyState → GL.ModifierKeys → InputEventMask
-- inputMaskKeys = InputEventMask ∘ GLFW.eventMaskKeys .:: GLFW.KeyEventMask
inputMaskKey k kss mks = InputEventMask $ GLFW.eventMaskKeys $ GLFW.KeyEventMask (Set.singleton k) kss mks

inputMaskKeyPress ∷ GL.Key → GL.ModifierKeys → InputEventMask
-- inputMaskKeys = InputEventMask ∘ GLFW.eventMaskKeys .:: GLFW.KeyEventMask
inputMaskKeyPress k mks = InputEventMask $ GLFW.eventMaskKeys $ GLFW.KeyEventMask (Set.singleton k) (Set.singleton GL.KeyState'Pressed) mks

inputMaskKeyRelease ∷ GL.Key → GL.ModifierKeys → InputEventMask
-- inputMaskKeys = InputEventMask ∘ GLFW.eventMaskKeys .:: GLFW.KeyEventMask
inputMaskKeyRelease k mks = InputEventMask $ GLFW.eventMaskKeys $ GLFW.KeyEventMask (Set.singleton k) (Set.singleton GL.KeyState'Released) mks

inputMaskChars   ∷ InputEventMask
inputMaskChars   = InputEventMask $ GLFW.eventMaskChars

inputMaskButtons ∷ GLFW.ButtonEventMask → InputEventMask
inputMaskButtons = InputEventMask ∘ GLFW.eventMaskButtons

inputMaskClick ∷ GL.MouseButton → GL.MouseButtonState → InputEventMask
inputMaskClick btn state = InputEventMask $ GLFW.eventMaskButtons $ GLFW.ButtonEventMask (Set.singleton btn) (Set.singleton state) mempty

inputMaskClick1Press, inputMaskClick1Release ∷ InputEventMask
inputMaskClick1Press   = inputMaskClick GL.MouseButton'1 GL.MouseButtonState'Pressed
inputMaskClick1Release = inputMaskClick GL.MouseButton'1 GL.MouseButtonState'Released

editMaskKeys ∷ InputEventMask
editMaskKeys = (inputMaskChars <>) $ InputEventMask $ GLFW.eventMaskKeys $ GLFW.KeyEventMask
  (Set.fromList
   [ GL.Key'Up, GL.Key'Down, GL.Key'Left, GL.Key'Right, GL.Key'Home, GL.Key'End
   , GL.Key'Backspace, GL.Key'Delete, GL.Key'Enter
   ])
  (Set.fromList [GL.KeyState'Pressed, GL.KeyState'Repeating])
  (mempty)


-- * Subscription of IdTokens to InputEventMasks
--
newtype Subscription = Subscription (MMap.MonoidalMap GLFW.EventType (Seq.Seq (IdToken, InputEventMask)))

instance Show Subscription where
  show (Subscription map) = ("(Subs"<>) ∘ (<>")") $ concat $
    [ " "<>show et<>"::"<> L.intercalate "+" [ printf "0x%x:%s" tok (show em)
                                             | (tokenHash → tok,(InputEventMask em)) ← toList subs]
    | (et, subs) ← MMap.toList map]

instance Semigroup Subscription where
  Subscription a <> Subscription b = Subscription $ a <> b

deriving newtype instance Monoid Subscription

subSingleton ∷ IdToken → InputEventMask → Subscription
subSingleton tok im@(InputEventMask em) = Subscription $
  MMap.fromList [ (evty, Seq.singleton (tok, im))
                | evty ← GLFW.eventMaskTypes em ]

subsByType ∷ Subscription → GLFW.EventType → Maybe (Seq.Seq (IdToken, InputEventMask))
subsByType (Subscription ss) ty = MMap.lookup ty ss

resolveSubs ∷ Reflex t ⇒ Input t → IdToken → SemSubs → Dynamic t Subscription
resolveSubs input tok (SemSubs ss) = inBinds input <&>
  (\binds→ L.foldl' (<>) mempty $
     subSingleton tok ∘ translateEvent binds <$> Set.toList ss)
