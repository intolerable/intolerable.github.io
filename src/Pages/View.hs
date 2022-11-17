module Pages.View where

import Data.Functor.Identity
import Lucid

type ViewM = Identity

class ToView a where
  toView :: a -> HtmlT ViewM ()
