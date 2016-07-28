{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ExplicitForAll #-}

module LN.UI.Core.Types (
  Array,
  List,
  Number,
  Tuple,
  tuple,
  String,
  Int,
  pattern Tuple,
  pattern Cons,
  pattern Nil,
  OrganizationName,
  UserName,
  ForumName,
  BoardName,
  ThreadName,
  ThreadPostName,
  OrganizationId,
  UserId,
  ForumId,
  BoardId,
  ThreadId,
  ThreadPostId,
  limitInt,
  View,
  View_
) where



import           Data.Int   (Int64)
import           Data.Text  (Text)
import           Prelude

import           LN.T.Param



type Array a   = [a]
type List a    = [a]
type Number    = Double
type Tuple a b = (a, b)



tuple :: a -> b -> (a, b)
tuple = (,)


pattern Tuple a b = (a, b)
pattern Cons a a' = (:) a a'
pattern Nil       = []



type OrganizationName = Text
type UserName         = Text
type ForumName        = Text
type BoardName        = Text
type ThreadName       = Text
type ThreadPostName   = Text



type OrganizationId = Int64
type UserId         = Int64
type ForumId        = Int64
type BoardId        = Int64
type ThreadId       = Int64
type ThreadPostId   = Int64



limitInt :: Int -> Param
limitInt = Limit . fromIntegral



type View a = forall m. (Monad m, Monoid (m a)) => m a
type View_  = View ()
