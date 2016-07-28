module LN.UI.Core.Router.Link (
  HasLink (..),
) where



import           LN.UI.Core.Router.Param (Params)
import           LN.UI.Core.Types        (Tuple)



class HasLink a where
  link :: a -> Tuple String Params
