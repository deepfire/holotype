module ExternalImports
  (module X
  )
where

-------------------------------------------------------- as X
import           Cardano.BM.Data.Aggregated              as X (Measurable (..))
import           Cardano.BM.Data.BackendKind             as X
import           Cardano.BM.Data.LogItem                 as X
import           Cardano.BM.Data.Output                  as X
import           Cardano.BM.Data.Severity                as X
import           Cardano.BM.Setup                        as X
import           Cardano.BM.Trace                        as X hiding (logDebug, logInfo, logNotice, logWarning, logError, logCritical, logAlert, logEmergency)
import           Control.Applicative                     as X
import           Control.Arrow                           as X (Arrow, ArrowApply, (***), (&&&), app, arr, first, second)
import           Control.Category                        as X hiding ((.), id, const)
import           Control.Exception                       as X (AsyncException, SomeException, assert, catch, fromException, throwIO)
-- import           Control.Lens                         as X hiding (As, children, from, to)
import           Control.Lens                            as X (Lens', Traversal', makeLenses, ix, _1, _2, (^.), (<&>), (.~), (-~), (+~), (%~), (&), _Just)
import           Control.Monad                           as X (foldM)
import           Control.Monad                           as X (unless, when, filterM)
import           Control.Monad.Fix                       as X
import           Control.Monad.IO.Class                  as X (MonadIO, liftIO)
import           Control.Monad.Plus                      as X (partial)
import           Control.Monad.Primitive                 as X
import           Control.Monad.Random                    as X hiding (lift)
import           Control.Monad.Reader                    as X (MonadReader, ask)
import           Control.Monad.Ref                       as X
import           Control.Monad.State                     as X hiding (lift)
import           Control.Monad.Trans                     as X
import           Control.Monad.Trans.Reader              as X (ReaderT, runReaderT)
import           Control.Newtype.Generics                as X (Newtype)
import           Data.Complex                            as X
import           Data.Either                             as X (either, fromRight, fromLeft, isLeft)
import           Data.Foldable                           as X (toList, foldr')
import           Data.Function                           as X (on)
import           Data.Functor                            as X
import           Data.Functor.Misc                       as X (Const2(..))
import           Data.GI.Base.ShortPrelude               as X (checkUnexpectedReturnNULL)
import           Data.Glb                                as X (HasGlb(..))
import           Data.IORef                              as X
import           Data.Kind                               as X (Type)
import           Data.List                               as X (cycle, find, intercalate, partition, sortOn)
import           Data.Lub                                as X (HasLub(..))
import           Data.Map.Strict                         as X (Map)
import           Data.Maybe                              as X (fromMaybe, isJust, fromJust)
import           Data.MonoTraversable                    as X
import           Data.Ord                                as X
import           Data.Proxy                              as X (Proxy(..))
--import           Data.Singletons                         as X hiding (TyCon(..), (<=))
import           Data.Singletons.TH                      as X (genSingletons)
import           Data.String                             as X (IsString(..))
import           Data.Text                               as X (Text, pack, unpack, toLower)
import           Data.Text.Format                        as X hiding (left, prec, right)
import           Data.Text.Lazy                          as X (fromStrict, toStrict)
import           Data.Text.Prettyprint.Doc               as X hiding (list)
import           Data.Text.Prettyprint.Doc.Render.Text   as X (renderLazy, renderStrict)
import           Data.Text.Zipper                        as X (TextZipper)
import           Data.Time.Clock                         as X
import           Data.Tuple                              as X
import           Data.Type.Bool                          as X
import           Data.Typeable                           as X (Typeable, typeRep)
import           Data.Vect                               as X (Mat4(..), Vec3(..), Vec4(..))
import           Debug.Trace                             as X (trace, traceIO, traceEventIO)
import           Foreign.Ptr                             as X
-- import           GHC.Base                                as X
import           GHC.Generics                            as X (Generic)
import           GHC.Num                                 as X
import           GHC.Stack                               as X (HasCallStack, callStack, prettyCallStack)
import           GHC.TypeLits                            as X
import           GHC.Types                               as X (Constraint, Type)
import           Generics.SOP                            as X (Code, Rep, HasDatatypeInfo, SList(..), SListI, I, K, All, All2, Top, POP(..), NP(..), SOP(..), NS(..))
import           Generics.SOP.NP                         as X (pure_NP)
import           Graphics.GL.Types                       as X
-- import           Linear                               as X hiding (Trace, V3, V4, basis, trace)
import           Linear                                  as X (Additive, V2(..), V3(..), V4(..), zero, _x, _y, (^+^), (^-^))
import           Numeric                                 as X
import           Numeric.Extra                           as X (doubleToFloat)
-- import           Prelude                              as X hiding (read, take, drop, length)
import           Prelude                                 as X hiding (Word, fail, id, words, print)
import           Prelude.Unicode                         as X
import           Reflex                                  as X hiding (Additive, Query, Query(..))
import           Reflex.GLFW                             as X (RGLFW)
import           Reflex.GLFW                             as X (RGLFW, InputU(..))
import           Reflex.GLFW                             as X (RGLFW, RGLFWGuest, InputU(..))
import           Reflex.Host.Class                       as X (ReflexHost, MonadReflexHost)
import           System.IO.Unsafe                        as X (unsafePerformIO)
import           Text.Printf                             as X (printf)
import           Text.Read                               as X (readPrec)
