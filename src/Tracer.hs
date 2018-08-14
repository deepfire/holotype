{-# LANGUAGE ViewPatterns, UnicodeSyntax #-}
module Tracer
  (trev, TraceKind(..), TraceEntity(..))
where
import           Debug.Trace
import           Foreign.Ptr
import           Numeric

data TraceKind
  = ALLOC
  | USE
  | FREE
  deriving (Eq, Show)

data TraceEntity
  = Tex
  deriving (Eq, Show)

trev ∷ Show a ⇒ TraceKind → TraceEntity → a → Ptr b → IO ()
trev kind entity arg (ptrToIntPtr → IntPtr addr) = do
  let msg = show kind <> " " <> show entity <> " " <> show arg <> " 0x" <> showHex (toInteger addr) ""
  traceIO      msg
  traceEventIO msg
