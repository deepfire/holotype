module Holo.Instances.Mutable
where

import           Data.Text                                (Text)
import           Data.Text.Zipper                         (TextZipper)
import           Prelude.Unicode
import           Reflex
import           Reflex.GLFW                              (RGLFW, InputU(..))
import qualified "GLFW-b" Graphics.UI.GLFW         as GLFW
import qualified Data.Text.Zipper                  as T
import qualified Reflex.GLFW                       as GLFW

import           Elsewhere
import {-# SOURCE #-}
                 Holo.Classes
import           Holo.Input


instance {-# OVERLAPPABLE #-} Mutable a where
  subscription = const mempty         -- declare ignorance..
  mutate       = immutable

immutable ∷ (RGLFW t m) ⇒ a → Event t Ev → m (Dynamic t a)
immutable init _ev = pure $ constDyn init


instance Mutable () where
  mutate = immutable


instance Mutable Bool where
  subscription tok _ = subSingleton tok inputMaskClick1Press
  mutate  initial ev = foldDyn (\_ v → not v) initial ev


instance Mutable Text where
  subscription tok _ = subSingleton tok editMaskKeys
  mutate initial ev =
    (zipperText <$>) <$> foldDyn (\Edit{..} tz → eeEdit tz) (textZipper [initial]) (translateEditEvent <$> ev)

data EditEvent where
  Edit ∷
    { eeEdit ∷ TextZipper Text → TextZipper Text
    } → EditEvent

translateEditEvent ∷ Ev → EditEvent
translateEditEvent (Ev ev) = case ev of
  (GLFWEv (U (GLFW.EventChar _ c)))                                              → Edit $ T.insertChar c
  (GLFWEv (U (GLFW.EventKey  _ GLFW.Key'Enter     _ GLFW.KeyState'Pressed   _))) → Edit $ T.breakLine
  (GLFWEv (U (GLFW.EventKey  _ GLFW.Key'Backspace _ GLFW.KeyState'Pressed   _))) → Edit $ T.deletePrevChar
  (GLFWEv (U (GLFW.EventKey  _ GLFW.Key'Delete    _ GLFW.KeyState'Pressed   _))) → Edit $ T.deleteChar
  (GLFWEv (U (GLFW.EventKey  _ GLFW.Key'Left      _ GLFW.KeyState'Pressed   _))) → Edit $ T.moveLeft
  (GLFWEv (U (GLFW.EventKey  _ GLFW.Key'Up        _ GLFW.KeyState'Pressed   _))) → Edit $ T.moveUp
  (GLFWEv (U (GLFW.EventKey  _ GLFW.Key'Right     _ GLFW.KeyState'Pressed   _))) → Edit $ T.moveRight
  (GLFWEv (U (GLFW.EventKey  _ GLFW.Key'Down      _ GLFW.KeyState'Pressed   _))) → Edit $ T.moveDown
  (GLFWEv (U (GLFW.EventKey  _ GLFW.Key'Home      _ GLFW.KeyState'Pressed   _))) → Edit $ T.gotoBOL
  (GLFWEv (U (GLFW.EventKey  _ GLFW.Key'End       _ GLFW.KeyState'Pressed   _))) → Edit $ T.gotoEOL
  (GLFWEv (U (GLFW.EventKey  _ GLFW.Key'Enter     _ GLFW.KeyState'Repeating _))) → Edit $ T.breakLine
  (GLFWEv (U (GLFW.EventKey  _ GLFW.Key'Backspace _ GLFW.KeyState'Repeating _))) → Edit $ T.deletePrevChar
  (GLFWEv (U (GLFW.EventKey  _ GLFW.Key'Delete    _ GLFW.KeyState'Repeating _))) → Edit $ T.deleteChar
  (GLFWEv (U (GLFW.EventKey  _ GLFW.Key'Left      _ GLFW.KeyState'Repeating _))) → Edit $ T.moveLeft
  (GLFWEv (U (GLFW.EventKey  _ GLFW.Key'Up        _ GLFW.KeyState'Repeating _))) → Edit $ T.moveUp
  (GLFWEv (U (GLFW.EventKey  _ GLFW.Key'Right     _ GLFW.KeyState'Repeating _))) → Edit $ T.moveRight
  (GLFWEv (U (GLFW.EventKey  _ GLFW.Key'Down      _ GLFW.KeyState'Repeating _))) → Edit $ T.moveDown
  (GLFWEv (U (GLFW.EventKey  _ GLFW.Key'Home      _ GLFW.KeyState'Repeating _))) → Edit $ T.gotoBOL
  (GLFWEv (U (GLFW.EventKey  _ GLFW.Key'End       _ GLFW.KeyState'Repeating _))) → Edit $ T.gotoEOL
  x → error $ "Unexpected event (non-edit): " <> show x
