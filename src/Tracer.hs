module Tracer
  ( module Cardano.BM.Trace
  , module Cardano.BM.Data.Aggregated
  , module Cardano.BM.Data.LogItem
  , mkTrace
  , logDebug,  logInfo,  logNotice,  logWarning,  logError,  logCritical,  logAlert,  logEmergency
  , logDebug', logInfo', logNotice', logWarning', logError', logCritical', logAlert', logEmergency'
  )
  -- (setupTracer, trev, trevE, TraceKind(..), TraceEntity(..), TraceAction(..))
where
import           Control.Monad
import           Control.Monad.IO.Class
import           GHC.Stack
import qualified Data.Map.Strict                   as Map
import qualified Data.Text.Format                  as TF
import qualified Data.Text.Format.Params           as TF
import           Debug.Trace
import           Data.List
import           Data.Text                               (Text)
import           Data.Text.Lazy                          (toStrict)
import           Data.Text.Format
import           Foreign.Ptr
import           Numeric
import           Prelude.Unicode
import qualified System.IO.Unsafe                  as IO
import qualified Data.IORef                        as IO

import           Control.Monad.Reader                     (MonadReader, ask)
import           Control.Monad.Trans
import           Control.Monad.Trans.Reader        hiding (ask)

import qualified Cardano.BM.Configuration.Model    as CM
import           Cardano.BM.Data.Aggregated (Measurable (..))
import           Cardano.BM.Data.BackendKind
import           Cardano.BM.Data.LogItem
import           Cardano.BM.Data.Output
import           Cardano.BM.Data.Severity
import           Cardano.BM.Setup
import           Cardano.BM.Trace                  hiding (logDebug, logInfo, logNotice, logWarning, logError, logCritical, logAlert, logEmergency)
import qualified Cardano.BM.Trace                  as T


-- * Tracing
--
class (MonadReader (Trace m) m, MonadIO m) ⇒ MonadTrace m

mkTrace ∷ MonadIO m ⇒ Text → m (Trace m)
mkTrace desc = do
    c  ← config
    -- create initial top-level |Trace|
    tr ← setupTrace (Right c) desc

    T.logNotice tr "-- tracing initialised"
    pure tr

-- runWithLogging ∷ (MonadIO m, MonadReader (Trace m) m, MonadIO m) ⇒ Text → m a → m a
-- runWithLogging desc act = do
--   tr ← mkTrace desc
--   runReader act tr

logDebug', logInfo', logNotice', logWarning', logError', logCritical', logAlert', logEmergency' ∷
  (MonadIO m, TF.Params ps) ⇒ Trace m → Format → ps → m ()
logDebug'     tr fmt ps = T.logDebug     tr $ toStrict $ TF.format fmt ps
logInfo'      tr fmt ps = T.logInfo      tr $ toStrict $ TF.format fmt ps
logNotice'    tr fmt ps = T.logNotice    tr $ toStrict $ TF.format fmt ps
logWarning'   tr fmt ps = T.logWarning   tr $ toStrict $ TF.format fmt ps
logError'     tr fmt ps = T.logError     tr $ toStrict $ TF.format fmt ps
logCritical'  tr fmt ps = T.logCritical  tr $ toStrict $ TF.format fmt ps
logAlert'     tr fmt ps = T.logAlert     tr $ toStrict $ TF.format fmt ps
logEmergency' tr fmt ps = T.logEmergency tr $ toStrict $ TF.format fmt ps

logDebug, logInfo, logNotice, logWarning, logError, logCritical, logAlert, logEmergency ∷
  (MonadReader (Trace m) m, MonadIO m, TF.Params ps) ⇒ Format → ps → m ()
logDebug         fmt ps = ask >>= \tr→ T.logDebug     tr $ toStrict $ TF.format fmt ps
logInfo          fmt ps = ask >>= \tr→ T.logInfo      tr $ toStrict $ TF.format fmt ps
logNotice        fmt ps = ask >>= \tr→ T.logNotice    tr $ toStrict $ TF.format fmt ps
logWarning       fmt ps = ask >>= \tr→ T.logWarning   tr $ toStrict $ TF.format fmt ps
logError         fmt ps = ask >>= \tr→ T.logError     tr $ toStrict $ TF.format fmt ps
logCritical      fmt ps = ask >>= \tr→ T.logCritical  tr $ toStrict $ TF.format fmt ps
logAlert         fmt ps = ask >>= \tr→ T.logAlert     tr $ toStrict $ TF.format fmt ps
logEmergency     fmt ps = ask >>= \tr→ T.logEmergency tr $ toStrict $ TF.format fmt ps


config ∷ MonadIO m ⇒ m CM.Configuration
config = liftIO $ do
    c ← CM.empty
    CM.setMinSeverity c Debug
    CM.setSetupBackends c [KatipBK, AggregationBK, EKGViewBK]
    -- per default each messages is sent to the logs, if not otherwise defined (see below: 'CM.setBackend')
    CM.setDefaultBackends c [KatipBK]
    CM.setSetupScribes c [ ScribeDefinition {
                              scName = "stdout"
                            , scKind = StdoutSK
                            , scRotation = Nothing
                            }
                      ]
    CM.setDefaultScribes c ["StdoutSK::stdout"]
    -- forward the random number to aggregation:
    CM.setBackend c "complex.random" (Just [AggregationBK])
    -- forward the aggregated output to the EKG view:
    CM.setBackend c "complex.random.aggregated" (Just [EKGViewBK])
    -- start EKG on http://localhost:12789
    CM.setEKGport c 1222

    return c

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
