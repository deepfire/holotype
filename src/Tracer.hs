{-# LANGUAGE GADTs, ViewPatterns, UnicodeSyntax #-}
module Tracer
  (setupTracer, trev, trevE, TraceKind(..), TraceEntity(..), TraceAction(..))
where
import           Control.Monad
import           Control.Monad.IO.Class
import           GHC.Stack
import qualified Data.Map.Strict                   as Map
import           Debug.Trace
import           Data.List
import           Foreign.Ptr
import           Numeric
import           Prelude.Unicode
import qualified System.IO.Unsafe                  as IO
import qualified Data.IORef                        as IO

data TraceKind
  = ALLOC
  | USE
  | MISSALLOC
  | REALLOC
  | FREE
  | REUSE
  | SIZE
  deriving (Eq, Ord, Show)

data TraceEntity
  = TEX
  | DRW
  | VIS
  | TOK
  | HOLO
  deriving (Eq, Ord, Show)

data TraceAction
  = IGNORE
  | STACK
  | TRACE
  deriving (Eq, Ord, Show)

data TraceConf where
  TraceConf ∷
    { defaultEnabled ∷ Bool
    , tcas ∷ Map.Map (TraceKind, TraceEntity) (TraceAction, Int)
    } → TraceConf

conf ∷ IO.IORef TraceConf
conf = IO.unsafePerformIO $ IO.newIORef $ TraceConf False mempty

setupTracer ∷ Bool → [(TraceKind, TraceEntity, TraceAction, Int)] → IO ()
setupTracer en = IO.writeIORef conf ∘ TraceConf en ∘ Map.fromList ∘ (fmap $ \(k,e,a,d)→((k,e),(a,d)))

trev ∷ (HasCallStack, MonadIO m, Show a) ⇒ TraceKind → TraceEntity → a → Int → m ()
trev kind entity arg addrOrId = liftIO $ do
  config ← IO.readIORef conf
  unless (Map.null $ tcas config) $ do
    let cfg = Map.lookup (kind, entity) $ tcas config
    let msg = show kind <> " " <> show entity <> " 0x" <> showHex addrOrId "" <> " " <> show arg
    case cfg of
      Just (IGNORE, _)     → pure ()
      Just (STACK,  depth) → let prefix = replicate depth ' ' in (putStrLn $ prettyCallStack callStack) >> traceIO (prefix <> msg) >> traceEventIO msg
      Just (TRACE,  depth) → let prefix = replicate depth ' ' in                                           traceIO (prefix <> msg) >> traceEventIO msg
      _                    →
        if defaultEnabled config
        then traceIO msg  >> traceEventIO msg
        else pure ()

trevE ∷ (HasCallStack, Show a) ⇒ TraceKind → TraceEntity → a → Int → b → b
trevE kind entity arg addrOrId x =
  let msg = show kind <> " " <> show entity <> " 0x" <> showHex addrOrId "" <> " " <> show arg
  in trace msg $ traceEvent msg x
