-- |

module Utils.Dijkstra where

type PQueue :: Type -> Type

emptyQ     :: PQueue a
singletonQ :: Ord a => a -> PQueue a
insertQ    :: Ord a => a -> PQueue a -> PQueue a

pattern EmptyQ :: PQueue a
pattern (:<)   :: Ord a => a -> PQueue a -> PQueue a


newtype PQueue a = PQueue (Set a)

emptyQ = PQueue Set.empty
singletonQ = PQueue . Set.singleton
insertQ x (PQueue s) = PQueue (Set.insert x s)

minView :: PQueue a -> Maybe (a, PQueue a)
minView (PQueue s) =
  case Set.minView s of
    Just (x, s') -> Just (x, PQueue s')
    Nothing      -> Nothing

pattern EmptyQ   <- (minView -> Nothing)
pattern (:<) x q <- (minView -> Just (x, q))
