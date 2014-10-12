
-- |
-- Module     : Simulation.Aivika.Trans.DoubleLinkedList
-- Copyright  : Copyright (c) 2009-2014, David Sorokin <david.sorokin@gmail.com>
-- License    : BSD3
-- Maintainer : David Sorokin <david.sorokin@gmail.com>
-- Stability  : experimental
-- Tested with: GHC 7.8.3
--
-- An imperative double-linked list.
--
module Simulation.Aivika.Trans.DoubleLinkedList 
       (DoubleLinkedList, 
        listNull, 
        listCount,
        newList, 
        listInsertFirst,
        listAddLast,
        listRemoveFirst,
        listRemoveLast,
        listFirst,
        listLast) where 

import Control.Monad

import Simulation.Aivika.Trans.Session
import Simulation.Aivika.Trans.ProtoRef
import Simulation.Aivika.Trans.Comp

-- | A cell of the double-linked list.
data DoubleLinkedItem m a = 
  DoubleLinkedItem { itemVal  :: a,
                     itemPrev :: ProtoRef m (Maybe (DoubleLinkedItem m a)),
                     itemNext :: ProtoRef m (Maybe (DoubleLinkedItem m a)) }
  
-- | The 'DoubleLinkedList' type represents an imperative double-linked list.
data DoubleLinkedList m a =  
  DoubleLinkedList { listSession :: Session m,
                     listHead :: ProtoRef m (Maybe (DoubleLinkedItem m a)),
                     listTail :: ProtoRef m (Maybe (DoubleLinkedItem m a)), 
                     listSize :: ProtoRef m Int }

-- | Test whether the list is empty.
listNull :: ProtoReferring m => DoubleLinkedList m a -> m Bool
{-# INLINE listNull #-}
listNull x =
  do head <- readProtoRef (listHead x) 
     case head of
       Nothing -> return True
       Just _  -> return False
    
-- | Return the number of elements in the list.
listCount :: ProtoReferring m => DoubleLinkedList m a -> m Int
{-# INLINE listCount #-}
listCount x = readProtoRef (listSize x)

-- | Create a new list.
newList :: ProtoReferring m => Session m -> m (DoubleLinkedList m a)
{-# INLINE newList #-}
newList s =
  do head <- newProtoRef s Nothing 
     tail <- newProtoRef s Nothing
     size <- newProtoRef s 0
     return DoubleLinkedList { listSession = s,
                               listHead = head,
                               listTail = tail,
                               listSize = size }

-- | Insert a new element in the beginning.
listInsertFirst :: ProtoReferring m => DoubleLinkedList m a -> a -> m ()
{-# INLINABLE listInsertFirst #-}
listInsertFirst x v =
  do let s = listSession x
     size <- readProtoRef (listSize x)
     writeProtoRef (listSize x) (size + 1)
     head <- readProtoRef (listHead x)
     case head of
       Nothing ->
         do prev <- newProtoRef s Nothing
            next <- newProtoRef s Nothing
            let item = Just DoubleLinkedItem { itemVal = v, 
                                               itemPrev = prev, 
                                               itemNext = next }
            writeProtoRef (listHead x) item
            writeProtoRef (listTail x) item
       Just h ->
         do prev <- newProtoRef s Nothing
            next <- newProtoRef s head
            let item = Just DoubleLinkedItem { itemVal = v,
                                               itemPrev = prev,
                                               itemNext = next }
            writeProtoRef (itemPrev h) item
            writeProtoRef (listHead x) item

-- | Add a new element to the end.
listAddLast :: ProtoReferring m => DoubleLinkedList m a -> a -> m ()
{-# INLINABLE listAddLast #-}
listAddLast x v =
  do let s = listSession x
     size <- readProtoRef (listSize x)
     writeProtoRef (listSize x) (size + 1)
     tail <- readProtoRef (listTail x)
     case tail of
       Nothing ->
         do prev <- newProtoRef s Nothing
            next <- newProtoRef s Nothing
            let item = Just DoubleLinkedItem { itemVal = v, 
                                               itemPrev = prev, 
                                               itemNext = next }
            writeProtoRef (listHead x) item
            writeProtoRef (listTail x) item
       Just t ->
         do prev <- newProtoRef s tail
            next <- newProtoRef s Nothing
            let item = Just DoubleLinkedItem { itemVal = v,
                                               itemPrev = prev,
                                               itemNext = next }
            writeProtoRef (itemNext t) item
            writeProtoRef (listTail x) item

-- | Remove the first element.
listRemoveFirst :: ProtoReferring m => DoubleLinkedList m a -> m ()
{-# INLINABLE listRemoveFirst #-}
listRemoveFirst x =
  do head <- readProtoRef (listHead x) 
     case head of
       Nothing ->
         error "Empty list: listRemoveFirst"
       Just h ->
         do size  <- readProtoRef (listSize x)
            writeProtoRef (listSize x) (size - 1)
            head' <- readProtoRef (itemNext h)
            case head' of
              Nothing ->
                do writeProtoRef (listHead x) Nothing
                   writeProtoRef (listTail x) Nothing
              Just h' ->
                do writeProtoRef (itemPrev h') Nothing
                   writeProtoRef (listHead x) head'

-- | Remove the last element.
listRemoveLast :: ProtoReferring m => DoubleLinkedList m a -> m ()
{-# INLINABLE listRemoveLast #-}
listRemoveLast x =
  do tail <- readProtoRef (listTail x) 
     case tail of
       Nothing ->
         error "Empty list: listRemoveLast"
       Just t ->
         do size  <- readProtoRef (listSize x)
            writeProtoRef (listSize x) (size - 1)
            tail' <- readProtoRef (itemPrev t)
            case tail' of
              Nothing ->
                do writeProtoRef (listHead x) Nothing
                   writeProtoRef (listTail x) Nothing
              Just t' ->
                do writeProtoRef (itemNext t') Nothing
                   writeProtoRef (listTail x) tail'

-- | Return the first element.
listFirst :: ProtoReferring m => DoubleLinkedList m a -> m a
{-# INLINABLE listFirst #-}
listFirst x =
  do head <- readProtoRef (listHead x)
     case head of
       Nothing ->
         error "Empty list: listFirst"
       Just h ->
         return $ itemVal h

-- | Return the last element.
listLast :: ProtoReferring m => DoubleLinkedList m a -> m a
{-# INLINABLE listLast #-}
listLast x =
  do tail <- readProtoRef (listTail x)
     case tail of
       Nothing ->
         error "Empty list: listLast"
       Just t ->
         return $ itemVal t
