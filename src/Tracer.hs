module Tracer
  ( module Cardano.BM.Trace
  , module Cardano.BM.Data.Aggregated
  , module Cardano.BM.Data.LogItem
  , mkTrace
  , Has(..)
  , MonadTrace, getTrace
  , logDebug,  logInfo,  logNotice,  logWarning,  logError,  logCritical,  logAlert,  logEmergency
  , logDebug', logInfo', logNotice', logWarning', logError', logCritical', logAlert', logEmergency'
  )
  -- (setupTracer, trev, trevE, TraceKind(..), TraceEntity(..), TraceAction(..))
where
import qualified Data.Map.Strict                   as Map
import qualified Data.Text.Format                  as TF
import qualified Data.Text.Format.Params           as TF
import qualified System.IO.Unsafe                  as IO
import qualified Data.IORef                        as IO
import qualified Cardano.BM.Configuration.Model    as CM
import qualified Cardano.BM.Trace                  as T
import           ExternalImports


-- * Tracing
--
class    (MonadIO m, MonadReader r m, Has (Trace IO) r) ⇒ MonadTrace r m
instance (MonadIO m, MonadReader r m, Has (Trace IO) r) ⇒ MonadTrace r m

mkTrace ∷ MonadIO m ⇒ Text → m (Trace m)
mkTrace desc = do
    c  ← config
    -- create initial top-level |Trace|
    tr ← setupTrace (Right c) desc

    T.logNotice tr "-- tracing initialised"
    pure tr

class Has t r where
  sliceReader ∷ r → t

getTrace ∷ MonadTrace r m ⇒ m (Trace IO)
getTrace = sliceReader <$> ask

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
  (MonadTrace r m, MonadIO m, TF.Params ps) ⇒ Format → ps → m ()
logDebug         fmt ps = getTrace >>= \tr→ liftIO $ T.logDebug     tr $ toStrict $ TF.format fmt ps
logInfo          fmt ps = getTrace >>= \tr→ liftIO $ T.logInfo      tr $ toStrict $ TF.format fmt ps
logNotice        fmt ps = getTrace >>= \tr→ liftIO $ T.logNotice    tr $ toStrict $ TF.format fmt ps
logWarning       fmt ps = getTrace >>= \tr→ liftIO $ T.logWarning   tr $ toStrict $ TF.format fmt ps
logError         fmt ps = getTrace >>= \tr→ liftIO $ T.logError     tr $ toStrict $ TF.format fmt ps
logCritical      fmt ps = getTrace >>= \tr→ liftIO $ T.logCritical  tr $ toStrict $ TF.format fmt ps
logAlert         fmt ps = getTrace >>= \tr→ liftIO $ T.logAlert     tr $ toStrict $ TF.format fmt ps
logEmergency     fmt ps = getTrace >>= \tr→ liftIO $ T.logEmergency tr $ toStrict $ TF.format fmt ps


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
