{-# LANGUAGE NoMonomorphismRestriction #-}
{-# OPTIONS_GHC  -fprint-potential-instances #-}
module Tracer
  ( module Cardano.BM.Trace
  , module Cardano.BM.Data.Aggregated
  , module Cardano.BM.Data.LogItem
  , MonadTrace, EffTrace
  , mkTrace, runWithTracing
  , logDebug,  logInfo,  logNotice,  logWarning,  logError,  logCritical,  logAlert,  logEmergency
  , logDebug', logInfo', logNotice', logWarning', logError', logCritical', logAlert', logEmergency'
  )
  -- (setupTracer, trev, trevE, TraceKind(..), TraceEntity(..), TraceAction(..))
where
import           Control.Effect                    hiding (Trace)
import           Control.Effect.Carrier
import           Control.Effect.Reader
import           Control.Effect.Lift
import           Control.Monad
import           Control.Monad.IO.Class
import qualified Control.Monad.Trans               as MTL
import qualified Control.Monad.Trans.Reader        as MTL
import           GHC.Stack
import qualified Data.Map.Strict                   as Map
import qualified Data.Text.Format                  as TF
import qualified Data.Text.Format.Params           as TF
import           Data.Kind                                (Type)
import           Data.List
import           Data.Text                               (Text)
import           Data.Text.Lazy                          (toStrict)
import           Data.Text.Format
import           Debug.Trace
import           Foreign.Ptr
import           Numeric
import           Prelude.Unicode
import qualified System.IO.Unsafe                  as IO
import qualified Data.IORef                        as IO

import qualified Cardano.BM.Configuration.Model    as CM
import           Cardano.BM.Data.Aggregated (Measurable (..))
import           Cardano.BM.Data.BackendKind
import           Cardano.BM.Data.LogItem
import           Cardano.BM.Data.Output
import           Cardano.BM.Data.Severity
import           Cardano.BM.Setup
import           Cardano.BM.Trace                  hiding (logDebug, logInfo, logNotice, logWarning, logError, logCritical, logAlert, logEmergency)
import qualified Cardano.BM.Trace                  as T

import           Cardano.BM.Data.Trace

-- * Tracing
--
type MonadTrace sig m  = (Member (Reader (Trace IO)) sig, Carrier sig m, MonadIO m)
type EffTrace       m a =    Eff (Reader (Trace m) m) a

runWithTracing ∷ ∀ r sig m a. (Carrier sig m, MonadIO m) ⇒ Text → Eff (ReaderC (Trace IO) m) a → m a
runWithTracing desc m = do
  tr ← liftIO $ mkTrace desc
  runReader tr m

mkTrace ∷ (MonadIO m) ⇒ Text → m (Trace m)
mkTrace desc = do
    c  ← config
    tr ← setupTrace (Right c) desc -- MonadIO m ⇒ Either FilePath Configuration → Text → m (Trace m)
    T.logNotice tr "-- tracing initialised"
    pure tr

-- subDoit ∷ (MonadIO m) ⇒ m (String, String)
-- subDoit = liftIO $ runM doit

-- sendM ∷ ∀ (sig ∷ (Type → Type) → Type → Type) (m ∷ Type → Type) (a ∷ k). sig a → m a
--sendM :: (Monad m, Monad (effect m), Member (Lift (effect m)) sig, Member effect sig, Carrier sig m) => effect m a -> m a
--sendM :: (Monad m, Monad (effect m), Member (Lift (effect m)) sig, Member effect sig, Carrier sig m) => effect m a -> m a

newtype WEff (n :: * -> *) m a = WEff { runWEff :: Eff m a }
  deriving (Applicative, Functor, Monad)

instance (Carrier sig m, Member (Lift n) sig, MonadIO n) => MonadIO (WEff n m) where
  liftIO = send . Lift . fmap pure . liftIO @n

instance Carrier sig m => Carrier sig (WEff n m) where
  ret = WEff . ret

sendM ∷ ∀ l m n sig a
  . ( Monad l
    , Monad m, Carrier sig m
    , Monad n
    , l ~ n
    , Member (Lift l) sig) ⇒ n a → m a
sendM m = send (Lift (fmap pure m))

doit ∷ ∀ m. (MonadIO m) ⇒ m (String, String)
doit =
  MTL.runReaderT (do
    liftIO $ runM $ runReader ("quux" ∷ String) $ do
      liftIO $ putStrLn "foo"
      foo ← sendM @(MTL.ReaderT String m) $ MTL.ask
      let foo = ""
      bar ← ask
      pure (foo, bar))
    ["bar"]

-- If you want to use fused-effects’ effects, you have to have a Carrier constraint
-- for the context and a Member constraint for the carrier’s signature. If you had
-- those for whatever monad you’re lifting with the Lift effect, then you wouldn’t
-- need the Lift effect at all.

-- Equivalently, a monad m lifted into a monad transformer t m can’t use t m’s
-- functionality; lift (action :: m a) :: t m a, i.e. action’s type doesn’t mention t
-- so it can’t do anything which requires t, only things which only require m.

-- If, on the other hand, you wanted to have some monad transformer wrapping a
-- fused-effects computation, you could do that with something like FooT (Eff
-- carrier). Then the outer context could be given a (possibly orphan) Carrier
-- instance and send requests to Eff, but the inner Eff context couldn’t send
-- requests to FooT (because it doesn’t contain it).

-- As to which layering is correct, that really depends on the semantics you want,
-- e.g. Eff (ErrorC e (Eff (StateC s m))) and Eff (StateC s (Eff (ErrorC e m))) have
-- different semantics: in the former, errors discard state, in the latter, errors do
-- not. (And the variations in meaning of layers of State carriers above and below
-- NonDet carriers are even more interesting.)


-- doit ∷ ∀ m sig. (MonadIO m) ⇒ m ()
-- doit = do
--   tr ∷ Trace IO ← liftIO $ mkTrace "yay"
--   MTL.runReaderT (do
--     liftIO $ runM $ runWithTracing "yay" $ do
--       liftIO $ putStrLn "foo"
--       foo ← MTL.ask
--       logDebug "we're live %s %s!" ("a" ∷ String, "b" ∷ String))
--     ["lol"]

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
  (MonadTrace sig m, TF.Params ps) ⇒ Format → ps → m ()
logDebug         fmt ps = ask >>= \tr→ liftIO . T.logDebug     tr . toStrict $ TF.format fmt ps
logInfo          fmt ps = ask >>= \tr→ liftIO . T.logInfo      tr . toStrict $ TF.format fmt ps
logNotice        fmt ps = ask >>= \tr→ liftIO . T.logNotice    tr . toStrict $ TF.format fmt ps
logWarning       fmt ps = ask >>= \tr→ liftIO . T.logWarning   tr . toStrict $ TF.format fmt ps
logError         fmt ps = ask >>= \tr→ liftIO . T.logError     tr . toStrict $ TF.format fmt ps
logCritical      fmt ps = ask >>= \tr→ liftIO . T.logCritical  tr . toStrict $ TF.format fmt ps
logAlert         fmt ps = ask >>= \tr→ liftIO . T.logAlert     tr . toStrict $ TF.format fmt ps
logEmergency     fmt ps = ask >>= \tr→ liftIO . T.logEmergency tr . toStrict $ TF.format fmt ps

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
