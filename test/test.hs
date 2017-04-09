-- Check unsafeCoerce usage in internals.

{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeApplications #-}

import Data.Proxy
import Data.Typeable
import Unsafe.Coerce

import qualified Data.TypeMap.Internal.Dynamic as TM
import qualified Data.TypeMap.Internal.Dynamic.Alt as TMA

idTypeRepTM :: TypeRep -> TypeRep
idTypeRepTM = TM.withTypeRep typeRep (Proxy @(TM.OfType TypeRep))

idTypeRepTMA :: TypeRep -> TypeRep
idTypeRepTMA = TMA.withTypeRep rep
  where
    rep :: forall t. Typeable t => TMA.Typed_ (TM.OfType TypeRep) t
    rep = TMA.Typed_ (typeRep (Proxy @t))

main = do
  assertEq listUnit (idTypeRepTM listUnit)
  assertEq listUnit (idTypeRepTMA listUnit)

listUnit :: TypeRep
listUnit = typeRep (Proxy :: Proxy [()])

assertEq :: (Eq a, Show a) => a -> a -> IO ()
assertEq a b
  | a == b = return ()
  | otherwise = putStrLn $ "Fail: " ++ show (a, b)
