module Kitten.Queue
  ( Queue
  , dequeue
  , empty
  , enqueue
  , fromList
  ) where

-- A generic queue with amortized O(1) enqueue/dequeue.

data Queue a = Queue [a] [a]

dequeue :: Queue a -> Maybe (a, Queue a)
dequeue (Queue i (x : o)) = Just (x, Queue i o)
dequeue (Queue i@(_ : _) []) = dequeue (Queue [] (reverse i))
dequeue (Queue [] []) = Nothing

empty :: Queue a
empty = Queue [] []

enqueue :: a -> Queue a -> Queue a
enqueue x (Queue i o) = Queue (x : i) o

fromList :: [a] -> Queue a
fromList = Queue [] . reverse
