{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UnicodeSyntax #-}

import           Prelude.Unicode
import           Control.Exception

catchAny ∷ IO a → (SomeException → IO a) → IO a
catchAny guarded handler = Control.Exception.catch guarded onExc
  where onExc e | shouldCatch e = handler e
                | otherwise = throwIO e
        shouldCatch e
          | show e ≡ "<<timeout>>" = False
          | Just (_ ∷ AsyncException) ← fromException e = False
          | otherwise = True
