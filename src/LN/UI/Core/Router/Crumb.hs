module LN.UI.Core.Router.Crumb (
  HasCrumb,
  crumb
) where



class HasCrumb a where
  crumb :: a -> [a]
