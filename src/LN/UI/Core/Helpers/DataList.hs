module LN.UI.Core.Helpers.DataList (
  deleteNth,
  tailFriendly,
  toSeqList
) where



import qualified Data.List   as List (splitAt, tail)
import           Data.Monoid ((<>))



deleteNth :: Int -> [a] -> [a]
deleteNth nth as = xs <> tailFriendly ys
  where
  (xs, ys) = List.splitAt nth as



tailFriendly :: [a] -> [a]
tailFriendly [] = []
tailFriendly xs = List.tail xs



toSeqList :: [a] -> [(Int, a)]
toSeqList = zip [0..]
