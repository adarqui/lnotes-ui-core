module LN.UI.Core.Helpers.Map (
  idmapFrom
) where



import qualified Data.List as List (zip)
import           Data.Map  (Map)
import qualified Data.Map  as Map



idmapFrom :: (Ord index) => (a -> index) -> [a] -> Map index a
idmapFrom un packs = Map.fromList $ List.zip (map un packs) packs
