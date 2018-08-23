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
    { tcas ∷ Map.Map (TraceKind, TraceEntity) (TraceAction, Int)
    } → TraceConf

conf ∷ IO.IORef TraceConf
conf = IO.unsafePerformIO $ IO.newIORef $ TraceConf mempty

setupTracer ∷ [(TraceKind, TraceEntity, TraceAction, Int)] → IO ()
setupTracer = IO.writeIORef conf ∘ TraceConf ∘ Map.fromList ∘ (fmap $ \(k,e,a,d)→((k,e),(a,d)))

trev ∷ (HasCallStack, MonadIO m, Show a) ⇒ TraceKind → TraceEntity → a → Int → m ()
trev kind entity arg addrOrId = liftIO $ do
  config ← tcas <$> IO.readIORef conf
  unless (Map.null config) $ do
    let cfg = Map.lookup (kind, entity) config
    let msg = show kind <> " " <> show entity <> " 0x" <> showHex addrOrId "" <> " " <> show arg
    case cfg of
      Just (IGNORE, _)     → pure ()
      Just (STACK,  depth) → let prefix = replicate depth ' ' in (putStrLn $ prettyCallStack callStack) >> traceIO (prefix <> msg) >> traceEventIO msg
      Just (TRACE,  depth) → let prefix = replicate depth ' ' in                                           traceIO (prefix <> msg) >> traceEventIO msg
      _                    →                                                                               traceIO            msg  >> traceEventIO msg

trevE ∷ (HasCallStack, Show a) ⇒ TraceKind → TraceEntity → a → Int → b → b
trevE kind entity arg addrOrId x =
  let msg = show kind <> " " <> show entity <> " 0x" <> showHex addrOrId "" <> " " <> show arg
  in trace msg $ traceEvent msg x
