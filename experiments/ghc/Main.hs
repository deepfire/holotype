{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeInType #-}
{-# LANGUAGE UndecidableSuperClasses #-}
{-# LANGUAGE UnicodeSyntax #-}
{-# OPTIONS_GHC -Wextra -Wno-unused-imports -Wno-unticked-promoted-constructors -Wno-type-defaults -Wno-missing-signatures #-}

module Main where

import           Control.Monad.IO.Class
import qualified GHC.Generics as GHC
import           Generics.SOP
import qualified Generics.SOP as SOP


class    ( GHC.Generic a, SOP.Generic a, HasDatatypeInfo a
         , forall xs. Code a ~ '[xs]) => Foo m a where

instance ( GHC.Generic a, SOP.Generic a, HasDatatypeInfo a
         , forall xs. Code a ~ '[xs]
         , Monad m)                  => Foo m a where

-- error:
--     • Could not deduce: Code a ~ '[xs]
--         arising from the superclasses of an instance declaration
--       from the context: (GHC.Generic a, HasDatatypeInfo a,
--                          forall (xs :: [*]). Code a ~ '[xs], Monad m)
--         bound by the instance declaration
--         at /run/user/1000/danteazsW1Q.hs:(22,10)-(24,47)
--     • In the instance declaration for ‘Foo m a’


main ∷ IO ()
main = do

  putStrLn "You are standing at the end of a road before a small brick building."
  putStrLn "Around you is a forest.  A small stream flows out of the building and"
  putStrLn "down a gully."
