module DevelMain (update) where

import Rapid
import Control.Wire.Controller (control)
import Holotype                (holotype)

update :: IO ()
update =
    rapid 0 $ \r ->
        restart r "holotype" $
          (control holotype)
