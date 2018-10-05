{-# LANGUAGE TypeFamilies, ConstraintKinds, ExistentialQuantification #-}
-- By Philip J-F, http://joyoftypes.blogspot.com/

import GHC.Types (Constraint)
import Control.Applicative

class RFunctor f where
  type C f :: * -> Constraint
  rfmap :: C f b => (a -> b) -> f a -> f b

class RFunctor f => RApplicative f where
  rpure :: C f a => a -> f a
  rzip :: f a -> f b -> f (a,b)

data UAp f a
  = Pure  a
  | forall b. Embed (f b) (b -> a)

toUAp :: C f a => f a -> UAp f a
toUAp x = Embed x id

fromUAp :: (RApplicative f, C f a) => UAp f a -> f a
fromUAp (Pure x) = rpure x
fromUAp (Embed x f) = rfmap f x

zipUAp :: RApplicative f => UAp f a -> UAp f b -> UAp f (a,b)
zipUAp (Pure a) (Pure b) = Pure (a,b)
zipUAp (Pure a) (Embed b f) = Embed b (\x -> (a,f x))
zipUAp (Embed a f) (Pure b) = Embed a (\x -> (f x,b))
zipUAp (Embed a f) (Embed b g) = Embed (rzip a b) (\(x,y) -> (f x,g y))

instance Functor (UAp f) where
  fmap f (Pure a) = Pure (f a)
  fmap f (Embed a g) = Embed a (f . g)

instance RApplicative f => Applicative (UAp f) where
  pure = Pure
  af <*> ax = fmap (\(f,x) -> f x) $ zipUAp af ax
  
