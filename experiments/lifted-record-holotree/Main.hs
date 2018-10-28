{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeInType #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE UnicodeSyntax #-}
{-# OPTIONS_GHC -Wextra -Wno-unused-imports -Wno-unticked-promoted-constructors -Wno-type-defaults -Wno-missing-signatures #-}

module Main where

import           Control.Compose
import           Control.Lens        hiding (children, pre)
import           Control.Monad.IO.Class
import           Data.Foldable
import           Data.Monoid
import           Data.Sequence              (Seq)
import qualified Data.Sequence           as S
import           Prelude.Unicode


(.:) = (.) ∘ (.)

data I x = I { unI ∷ x }

data HB
  = HB { _s ∷ String, _children ∷ [HB] }
  -- | HBE { _s ∷ String, _children ∷ [HB] }
  -- | HBR { _s ∷ String, _children ∷ [HB] }
  deriving (Show)
makeLenses ''HB

walk ∷ (a ~ HB, Monad m) ⇒ ([Int] → a → m ()) → a → m ()
walk action f = loop [0] f
  where loop trace f = do
          action trace f
          sequence $ zip (f^.children) [0..] <&>
            \(f', n) → loop (n:trace) f'
          pure ()

dump ∷ (a ~ HB, MonadIO m) ⇒ (a ~ HB ⇒ a → String) → a → m ()
dump ppf = walk
  (liftIO .: (\trace f' →
                 putStrLn $ (concat $ take (length trace - 1) (repeat "  "))
                 <> (show $ head trace) <> " " <> ppf f'))

data Derived x = W (x, HB)                           -- Derived ~ W ~ Dynamic
  deriving Show

instance Functor     Derived where
  fmap f (W (x, xs)) = W (f x, xs)

instance Applicative Derived where
  pure x = W (x, HB "vboxEnd" [])
  W (f, fhb@(HB fhbn _)) <*> W (x, (HB xhbn xhbr)) =
    W (f x, HB ("(ER "<>fhbn<>xhbn<> ")") (fhb : xhbr))
  -- W (_, (HB l _)) <*> W (_, (HB r _)) = error $ "HB "<>l<>" <*> HB "<>r
  -- W (_, (HB l _)) <*> W (_, (HB r _)) = error $ "HB "<>l<>" <*> HB "<>r
  -- W (_, (HB l _)) <*> W (_, (HB r _)) = error $ "HB "<>l<>" <*> HB "<>r
  -- W (_, (HB l _)) <*> W (_, (HB r _)) = error $ "HB "<>l<>" <*> HB "<>r

  -- pure x = W (mempty, constDyn (x, Holo.vbox []))
  -- W (fsubs, fvals) <*> W (xsubs, xvals) =
  --   W $ (,)
  --   (zipDynWith (<>) fsubs xsubs)
  --   (zipDynWith ((\(f,   fhb@Item{..})
  --                  (  x,                                   xhb)→
  --                  (f x, fhb { hiChildren = hiChildren <> [xhb] })))
  --     fvals xvals)

class (MonadIO m, Show a) ⇒ Liftable m a where
  lift ∷ a → (m :. Derived) a
  lift x = O ∘ pure ∘ W $ (x,
                           HB "hbox" [ HB "val" []
                                      , HB (show x) []])
instance MonadIO m ⇒ Liftable m String

type C = []
fromL ∷ [x] → C x
fromL = id
toL   ∷ C x → [x]
toL   = id
data Travable f x = Travable (C (f x))

pre  ∷ (Monad m, x ~ String, m ~ IO) ⇒ Travable (m :. Derived) x
pre  = Travable $ fromL $ lift <$> ["a", "b", "c", "d"
                                    -- , "e", "f", "g", "h", "i", "j", "k"
                                   ]

tsequence ∷ Applicative f ⇒ Travable f x → f (Travable I x)
tsequence (Travable s) = Travable ∘ (I <$>) <$> sequenceA s

post ∷ (Monad m, x ~ String, m ~ IO) ⇒ (m :. Derived) (Travable I x)
post = tsequence pre


main ∷ IO ()
main = do
  let O act = post
  (W (Travable s, hb)) ← act
  putStrLn $ show $ unI <$> toL s
  dump _s hb
