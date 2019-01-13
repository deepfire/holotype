{-# OPTIONS_GHC -Wextra #-}
{-# OPTIONS_GHC -Wno-implicit-prelude -Wno-missing-import-lists -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unsafe #-}
module Elsewhere
  ( textZipper, zipperText
  , everything
  , proxy
  , trEv, trDyn, trDynM
  , avgStep, average
  , goldenRatio
  , (.:)
  , rcomp
  , flip2
  , choosePartially
  , catchAny
  , partial
  , showTime
  , timeDiff
  , printTimeDiff
  , (<:)
  )
where

import           Control.Applicative
import           Control.Exception                        (AsyncException, SomeException, catch, fromException, throwIO)
import           Control.Monad.Plus                       (partial)
import           Control.Monad.Random
import           Control.Monad.State
import           Data.Complex
import           Data.Glb                                 (HasGlb(..))
import           Data.Lub                                 (HasLub(..))
import           Data.Maybe                               (fromMaybe)
import           Data.Proxy                               (Proxy(..))
import           Data.Time.Clock
import           Data.Typeable                            (Typeable)
import qualified Data.TypeMap.Dynamic              as TM
import           GHC.Stack                                ()
import           Linear                            hiding (trace)
import           Prelude.Unicode
import           Reflex
import           Text.Printf                              (printf)
import qualified Data.Text                         as T
import qualified Data.Text.Zipper                  as T


-- * 'lub' + 'linear'
instance Ord a ⇒ HasLub (V2 a) where lub = liftA2 max
instance Ord a ⇒ HasGlb (V2 a) where glb = liftA2 min


-- * 'base' + 'random'
--
instance Random a ⇒ Random (Complex a) where
  randomR (r:+i, r':+i') = runState $ liftA2 (:+) (state $ randomR (r,r')) (state $ randomR (i,i'))
  random                 = runState $ liftA2 (:+) (state $ random)         (state $ random)


-- * Text.Zipper
--
textZipper ∷ [T.Text] → T.TextZipper T.Text
textZipper = flip T.textZipper Nothing

zipperText ∷ T.TextZipper T.Text → T.Text
zipperText = T.dropEnd 1 ∘ T.unlines ∘ T.getText


-- * Reflex
--
trEv ∷  Reflex t ⇒ String → Event   t a → Event t a
trEv = traceEventWith ∘ const

trDyn ∷ Reflex t ⇒ String → Dynamic t a → Dynamic t a
trDyn = traceDynWith ∘ const

trDynM ∷ Reflex t ⇒ String → String → Dynamic t (Maybe a) → Dynamic t (Maybe a)
trDynM no ju = traceDynWith (\case; Nothing → no; Just _ → ju)

type Avg a = (Int, Int, [a])
avgStep ∷ Fractional a ⇒ a → (a, Avg a) → (a, Avg a)
avgStep x (_, (lim, cur, xs)) =
  let (ncur, nxs) = if cur < lim
                    then (cur + 1, x:xs)
                    else (lim,     x:Prelude.init xs)
  in ((sum nxs) / fromIntegral ncur, (lim, ncur, nxs))

average ∷ (Fractional a, Reflex t, MonadHold t m, MonadFix m) ⇒ Int → Event t a → m (Dynamic t a)
average n e = (fst <$>) <$> foldDyn avgStep (0, (n, 0, [])) e


-- * Pretty numbers
--
goldenRatio ∷ Double
goldenRatio = 1.61803398875


-- * Generic functions
--
(.:) ∷ ∀ a f g b. (b → a) → (f → g → b) → f → g → a
(.:) = (.) ∘ (.)
{-# INLINE (.:) #-}

infixr 9 .:

rcomp ∷ (a → b) → (b → c) → (a → c)
rcomp = flip (.)
{-# INLINE rcomp #-}

flip2 ∷ (a → b → c → d) → b → c → a → d
flip2 f b c a = f a b c
{-# INLINE flip2 #-}

choosePartially ∷ Eq a ⇒ a → a → a → a
choosePartially one l r = fromMaybe one $ partial (≢ one) l <|> partial (≢ one) r

everything :: (Enum a, Bounded a) => [a]
everything = enumFromTo minBound maxBound

proxy ∷ a → Proxy a
proxy = const Proxy
{-# INLINE proxy #-}


-- * Exceptions
--
catchAny ∷ IO a → (SomeException → IO a) → IO a
catchAny guarded handler = Control.Exception.catch guarded onExc
  where onExc e | shouldCatch e = handler e
                | otherwise = throwIO e
        shouldCatch e
          | show e ≡ "<<timeout>>" = False
          | Just (_ ∷ AsyncException) ← fromException e = False
          | otherwise = True


-- * Simple benchmarking functions from lambdacube-quake3
--
showTime ∷ NominalDiffTime → String
showTime delta
    | t > 1e-1  = printf "%.3fs" t
    | t > 1e-3  = printf "%.1fms" (t/1e-3)
    | otherwise = printf "%.0fus" (t/1e-6)
  where
    t = realToFrac delta :: Double

timeDiff ∷ IO a → IO (NominalDiffTime, a)
timeDiff m = (\s x e -> (diffUTCTime e s, x))
  <$> getCurrentTime
  <*> m
  <*> getCurrentTime

printTimeDiff ∷ String → IO a → IO a
printTimeDiff message m = do
  (t,r) <- timeDiff m
  putStr message
  putStrLn $ showTime t
  return r


-- * In progress
--
(<:) ∷ Typeable b ⇒ TM.TypeMap a → (Proxy b, TM.Item a b) → TM.TypeMap a
(<:) tm (k, v) = TM.insert k v tm
