{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE PackageImports    #-}
{-# LANGUAGE RebindableSyntax #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

-- | A Queue is a structure similar to a List, but elements are added to
-- one end (the back) and retrieved from the other end (the front).
-- Thus, a Queue is a FIFO (first-in first-out) structure, just like waiting
-- lists that are served in the order of arrival.

module Extras.Queue(
  -- $intro
  Queue,qNew,qLength,qFront,qRest,qAppended,qNumber
  ) where
  
import Prelude

----------------------------------------------------------------------------
-- $intro
-- = A Queue
--
-- To use the extra features in this module, you must begin your code with
-- this line:
--
-- > import Extras.Queue
--

-- | A fresh new, empty Queue with the given Number as its identifier.
-- Identifiers are arbitrary numbers that you can use to distinguish
-- between several queues in your code.
qNew :: Number -> Queue a
qNew = newQueue

-- | The number of elements currently waiting in the given Queue
qLength :: Queue a -> Number
qLength = qlen

-- | @qFront(queue,default)@ is either the front element at @queue@ (when
-- it is not empty) or @default@ (when the @queue@ is empty)
qFront :: (Queue a, a) -> a
qFront = qtop

-- | A Queue like the given queue, but with the front element removed.
-- If the given queue is empty, it remains unchanged.
qRest :: Queue a -> Queue a
qRest = qpop

-- | A Queue like the given queue, but with the given element added to the
-- back
qAppended :: (a, Queue a) -> Queue a
qAppended = qpush

-- | The identifier associated to the given Queue
qNumber :: Queue a -> Number
qNumber = qid

-- | The internal structure of a @Queue@ is not exposed. You need to use
-- the functions in this module to manipulate it.
data Queue a = Queue
  { qi :: [a]
  , qo :: [a]
  , qlen :: Number
  , qid :: Number
  }

newQueue(i) = Queue { qi = [], qo = [], qlen = 0 , qid = i }

rebuilt(q@Queue{..})
  | qlen <= 0 = q
  | empty(qo) = rebuild(q)
  | otherwise = q
  where
  rebuild(qq) = qq { qo = reversed(qi) , qi = [] }

qpush(x,q@Queue{..}) = q'.#rebuilt
  where
  q' = q { qi = x:qi , qlen = 1 + qlen }

qpop(q)
  | qlen(q) <= 0 = q
  | otherwise = q'.#rebuilt
  where
  q' = q { qo = rest(q.#qo,1) , qlen = q.#qlen - 1 }
  
qtop(q,d)
  | qlen(q) <= 0 = d
  | otherwise = q.#qo#1

x .# f = f(x)
