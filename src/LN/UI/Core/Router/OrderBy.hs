module LN.UI.Core.Router.OrderBy (
  HasOrderBy,
  orderBy
) where



import           LN.T             (OrderBy)
import           LN.UI.Core.Types (Array)



class HasOrderBy a where
  orderBy :: a -> Array OrderBy
