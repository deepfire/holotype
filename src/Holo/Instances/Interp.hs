module Holo.Instances.Interp
where

import           Data.Text                                (Text)
import qualified Data.Text                         as T
import           Linear.V2                                (V2(..))
import           Prelude.Unicode
import           Text.Read                                (readMaybe)

import           Graphics.Flatland

import {-# SOURCE #-}
                 Holo.Classes
import qualified Holo.Port                         as Port


instance {-# OVERLAPS #-} Interp Text Text where
  interp        = Just
  forget        = id

instance {-# INCOHERENT #-} Interp a a where
  interp        = Just
  forget        = id

instance {-# INCOHERENT #-} (Read b, Show b) ⇒ Interp Text b where
  interp        = readMaybe ∘ T.unpack
  forget        = T.pack ∘ show

instance (Interp a b, Interp c d) ⇒ Interp (a, c) (b, d) where
  interp (a,c) = (,) <$> interp a <*> interp c
  forget (b,d) = (,) (forget b) (forget d)

instance Interp (a, a)  (V2 a) where
  interp (x, y)   = Just (V2 x y)
  forget (V2 x y) = (x, y)

instance Interp (a, a)  (Di a) where
  interp (x, y)        = Just (Di (V2 x y))
  forget (Di (V2 x y)) = (x, y)

instance Interp (a, a)  (Port.ScreenDim (Di a)) where
  interp (x, y)        = Just (Port.ScreenDim (Di (V2 x y)))
  forget (Port.ScreenDim (Di (V2 x y))) = (x, y)

instance Interp Bool Port.WaitVSync where
  interp               = Just ∘ Port.WaitVSync
  forget (Port.WaitVSync x) = x
