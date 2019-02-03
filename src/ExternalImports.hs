module ExternalImports where

import           Cardano.BM.Data.Aggregated (Measurable (..))
import           Cardano.BM.Data.BackendKind
import           Cardano.BM.Data.LogItem
import           Cardano.BM.Data.Output
import           Cardano.BM.Data.Severity
import           Cardano.BM.Setup
import           Cardano.BM.Trace                  hiding (logDebug, logInfo, logNotice, logWarning, logError, logCritical, logAlert, logEmergency)
import           Control.Applicative
import           Control.Arrow                            ((***), (&&&))
import           Control.Category                  hiding ((.), id, const)
import           Control.Exception                        (AsyncException, SomeException, assert, catch, fromException, throwIO)
-- import           Control.Lens                      hiding (As, children, from, to)
import           Control.Lens                             (Lens', Traversal', makeLenses, ix, _1, (^.), (<&>), (.~), (-~), (+~), (%~))
import           Control.Monad.Fix
import           Control.Monad                            (foldM)
import           Control.Monad.IO.Class                   (MonadIO, liftIO)
import           Control.Monad.Plus                       (partial)
import           Control.Monad.Primitive
import           Control.Monad.Random              hiding (lift)
import           Control.Monad.Reader                     (MonadReader, ask)
import           Control.Monad.Ref
import           Control.Monad.State               hiding (lift)
import           Control.Monad.Trans
import           Control.Monad.Trans.Reader               (ReaderT)
import           Control.Monad                            (unless, when, filterM)
import           Control.Newtype.Generics
import           Data.Complex
import           Data.Either                              (either)
import           Data.Foldable                            (toList, foldr')
import           Data.Function                            (on)
import           Data.Functor
import           Data.Functor.Misc                        (Const2(..))
import           Data.GI.Base.ShortPrelude                (checkUnexpectedReturnNULL)
import           Data.Glb                                 (HasGlb(..))
import           Data.Lub                                 (HasLub(..))
import           Data.IORef
import           Data.Kind                                (Type)
import           Data.List                                (cycle)
import           Data.Lub                                 (HasLub(..))
import           Data.Map.Strict                          (Map)
import           Data.Maybe                               (fromMaybe, isJust)
import           Data.MonoTraversable
import           Data.Ord
import           Data.Proxy                               (Proxy(..))
import           Data.Singletons
import           Data.Singletons.TH                hiding ((%~))
import           Data.String                              (IsString(..))
import           Data.Text                         as T   (Text, unpack)
import           Data.Text.Format                  hiding (prec)
import           Data.Text.Lazy                          (toStrict)
import           Data.Text.Prettyprint.Doc
import           Data.Text.Prettyprint.Doc.Render.Text    (renderLazy)
import           Data.Text                                (Text, pack, unpack, toLower, drop)
import           Data.Text.Zipper                         (TextZipper)
import           Data.Time.Clock
import           Data.Tuple
import           Data.Typeable                            (Typeable, typeRep)
import           Data.Type.Bool
import           Data.Vect                                (Mat4(..), Vec3(..), Vec4(..))
import           Debug.Trace                              (trace)
import           Foreign.Ptr
import           Generics.SOP                      hiding (Generic, from)
import           Generics.SOP.NP                          (pure_NP)
import           Generics.SOP                             (Top)
import           GHC.Base
import           GHC.Generics                             (Generic)
import           GHC.Num
import           GHC.Stack                                (HasCallStack)
import           GHC.TypeLits
import           GHC.Types                                (Constraint, Type)
import "GLFW-b"  Graphics.UI.GLFW                  as GL
import           Graphics.GL.Core33                as GL
import           LambdaCube.Mesh                   as LC
-- import           Linear                            hiding (Trace, V3, V4, basis, trace)
import           Linear                                   (Additive, V2(..), V3(..), zero)
import           Numeric
import           Numeric.Extra                            (doubleToFloat)
import           Prelude                           hiding (Word, fail, id, words)
-- import           Prelude                           hiding (read, take, drop, length)
import           Prelude.Unicode
import           Reflex.GLFW                              (RGLFW)
import           Reflex.GLFW                              (RGLFW, InputU(..))
import           Reflex.GLFW                              (RGLFW, RGLFWGuest, InputU(..))
import           Reflex                            hiding (Additive, Query, Query(..))
import           Reflex.Host.Class                        (ReflexHost, MonadReflexHost)
import           System.IO.Unsafe                         (unsafePerformIO)
import           Text.Printf                              (printf)
import           Text.Read                         hiding (prec)

import qualified Cardano.BM.Configuration.Model    as CM
import qualified Cardano.BM.Trace                  as T
import qualified Codec.Picture                     as Juicy
import qualified Codec.Picture.Saving              as Juicy
import qualified Control.Category                  as C
import qualified Control.Concurrent.STM            as STM
import qualified Control.Monad.Ref
import qualified Data.Aeson.Encode.Pretty          as AE
import qualified Data.ByteString                   as B
import qualified Data.ByteString.Char8             as SB
import qualified Data.ByteString.Lazy              as LB
import qualified Data.GI.Base                      as GI
import qualified Data.GI.Base.CallStack            as B.CallStack
import qualified Data.IntMap.Strict                as IntMap
import qualified Data.IORef                        as IO
import qualified Data.Label.Mono                   as DLM
import qualified Data.Label.Point                  as DLP
import qualified Data.List                         as L
import qualified Data.Map.Monoidal.Strict          as MMap
import qualified Data.Map.Strict                   as Map
import qualified Data.Sequence                     as Seq
import qualified Data.Set                          as Set
import qualified Data.Text                         as T
import qualified Data.Text.Format                  as T
import qualified Data.Text.Format                  as TF
import qualified Data.Text.Format.Params           as TF
import qualified Data.Text.Lazy                    as TL
import qualified Data.Text.Lazy.IO                 as TLIO
import qualified Data.Text.Zipper                  as T
import qualified Data.Time.Clock                   as Time
import qualified Data.TypeMap.Dynamic              as TM
import qualified Data.Vect                         as Vc
import qualified Data.Vector                       as V
import qualified Data.Vector.Storable.ByteString   as B
import qualified Foreign                           as F
import qualified Foreign.Concurrent                as FC
import qualified Foreign.C.Types                   as F
import qualified Foreign.ForeignPtr.Unsafe         as F
import qualified Generics.SOP                      as SOP
import qualified Generics.SOP.NP                   as SOP
import qualified GHC.Generics                      as GHC
import qualified GHC.Stats                         as Sys
import qualified GI.Cairo                          as GIC
import qualified GI.GObject.Objects.Object         as GI
import qualified GI.Pango                          as GIP
import qualified GI.PangoCairo.Functions           as GIPC
import qualified GI.PangoCairo.Interfaces.FontMap  as GIPC
import qualified "GLFW-b" Graphics.UI.GLFW         as GLFW
import qualified Graphics.GL.Core33                as GL
import qualified Graphics.Rendering.Cairo          as GRC
import qualified Graphics.Rendering.Cairo.Internal as GRCI
import qualified Graphics.Rendering.Cairo.Internal as GRC (Render(..), create, destroy)
import qualified Graphics.Rendering.Cairo.Types    as GRC
import qualified LambdaCube.Compiler               as GL
import qualified LambdaCube.GL                     as GL
import qualified LambdaCube.GL.Mesh                as GL
import qualified LambdaCube.GL.Type                as GL
import qualified LambdaCube.Linear                 as LCLin
import qualified Options.Applicative               as Opt
import qualified Reflex.GLFW                       as GLFW
import qualified System.Clock                      as Sys
import qualified System.IO                         as Sys
import qualified System.IO.Unsafe                  as IO
import qualified System.Mem                        as Sys
import qualified Text.Parser.Char                  as P
import qualified Text.Parser.Combinators           as P
import qualified Text.Parser.Token                 as P
import qualified Text.Read                         as TR
import qualified Text.Trifecta.Parser              as P
import qualified Text.Trifecta.Result              as P
import qualified Unsafe.Coerce                     as Co
