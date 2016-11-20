
-- |
-- Module     : Simulation.Aivika.DoubleLinkedList
-- Copyright  : Copyright (c) 2009-2016, David Sorokin <david.sorokin@gmail.com>
-- License    : BSD3
-- Maintainer : David Sorokin <david.sorokin@gmail.com>
-- Stability  : experimental
-- Tested with: GHC 8.0.1
--
-- An imperative double-linked list.
--
module Simulation.Aivika.DoubleLinkedList 
       (DoubleLinkedList, 
        listNull, 
        listCount,
        newList, 
        listInsertFirst,
        listAddLast,
        listRemoveFirst,
        listRemoveLast,
        listRemove,
        listRemoveBy,
        listContains,
        listContainsBy,
        listFirst,
        listLast,
        clearList,
        freezeList) where 

import Data.IORef
import Data.Maybe

import Control.Monad

-- | A cell of the double-linked list.
data DoubleLinkedItem a = 
  DoubleLinkedItem { itemVal  :: a,
                     itemPrev :: IORef (Maybe (DoubleLinkedItem a)),
                     itemNext :: IORef (Maybe (DoubleLinkedItem a)) }
  
-- | The 'DoubleLinkedList' type represents an imperative double-linked list.
data DoubleLinkedList a =  
  DoubleLinkedList { listHead :: IORef (Maybe (DoubleLinkedItem a)),
                     listTail :: IORef (Maybe (DoubleLinkedItem a)), 
                     listSize :: IORef Int }

-- | Test whether the list is empty.
listNull :: DoubleLinkedList a -> IO Bool
listNull x =
  do head <- readIORef (listHead x) 
     case head of
       Nothing -> return True
       Just _  -> return False
    
-- | Return the number of elements in the list.
listCount :: DoubleLinkedList a -> IO Int
listCount x = readIORef (listSize x)

-- | Create a new list.
newList :: IO (DoubleLinkedList a)
newList =
  do head <- newIORef Nothing 
     tail <- newIORef Nothing
     size <- newIORef 0
     return DoubleLinkedList { listHead = head,
                               listTail = tail,
                               listSize = size }

-- | Insert a new element in the beginning.
listInsertFirst :: DoubleLinkedList a -> a -> IO ()
listInsertFirst x v =
  do size <- readIORef (listSize x)
     let size' = size + 1
     size' `seq` writeIORef (listSize x) size'
     head <- readIORef (listHead x)
     case head of
       Nothing ->
         do prev <- newIORef Nothing
            next <- newIORef Nothing
            let item = Just DoubleLinkedItem { itemVal = v, 
                                               itemPrev = prev, 
                                               itemNext = next }
            writeIORef (listHead x) item
            writeIORef (listTail x) item
       Just h ->
         do prev <- newIORef Nothing
            next <- newIORef head
            let item = Just DoubleLinkedItem { itemVal = v,
                                               itemPrev = prev,
                                               itemNext = next }
            writeIORef (itemPrev h) item
            writeIORef (listHead x) item

-- | Add a new element to the end.
listAddLast :: DoubleLinkedList a -> a -> IO ()
listAddLast x v =
  do size <- readIORef (listSize x)
     let size' = size + 1
     size' `seq` writeIORef (listSize x) size'
     tail <- readIORef (listTail x)
     case tail of
       Nothing ->
         do prev <- newIORef Nothing
            next <- newIORef Nothing
            let item = Just DoubleLinkedItem { itemVal = v, 
                                               itemPrev = prev, 
                                               itemNext = next }
            writeIORef (listHead x) item
            writeIORef (listTail x) item
       Just t ->
         do prev <- newIORef tail
            next <- newIORef Nothing
            let item = Just DoubleLinkedItem { itemVal = v,
                                               itemPrev = prev,
                                               itemNext = next }
            writeIORef (itemNext t) item
            writeIORef (listTail x) item

-- | Remove the first element.
listRemoveFirst :: DoubleLinkedList a -> IO ()
listRemoveFirst x =
  do head <- readIORef (listHead x) 
     case head of
       Nothing ->
         error "Empty list: listRemoveFirst"
       Just h ->
         do size  <- readIORef (listSize x)
            let size' = size - 1
            size' `seq` writeIORef (listSize x) size'
            head' <- readIORef (itemNext h)
            case head' of
              Nothing ->
                do writeIORef (listHead x) Nothing
                   writeIORef (listTail x) Nothing
              Just h' ->
                do writeIORef (itemPrev h') Nothing
                   writeIORef (listHead x) head'

-- | Remove the last element.
listRemoveLast :: DoubleLinkedList a -> IO ()
listRemoveLast x =
  do tail <- readIORef (listTail x) 
     case tail of
       Nothing ->
         error "Empty list: listRemoveLast"
       Just t ->
         do size  <- readIORef (listSize x)
            let size' =  size - 1
            size' `seq` writeIORef (listSize x) size'
            tail' <- readIORef (itemPrev t)
            case tail' of
              Nothing ->
                do writeIORef (listHead x) Nothing
                   writeIORef (listTail x) Nothing
              Just t' ->
                do writeIORef (itemNext t') Nothing
                   writeIORef (listTail x) tail'

-- | Return the first element.
listFirst :: DoubleLinkedList a -> IO a
listFirst x =
  do head <- readIORef (listHead x)
     case head of
       Nothing ->
         error "Empty list: listFirst"
       Just h ->
         return $ itemVal h

-- | Return the last element.
listLast :: DoubleLinkedList a -> IO a
listLast x =
  do tail <- readIORef (listTail x)
     case tail of
       Nothing ->
         error "Empty list: listLast"
       Just t ->
         return $ itemVal t

-- | Remove the specified element from the list and return a flag
-- indicating whether the element was found and removed.
listRemove :: Eq a => DoubleLinkedList a -> a -> IO Bool
listRemove x v = fmap isJust $ listRemoveBy x (== v)

-- | Remove an element satisfying the specified predicate and return
-- the element if found.
listRemoveBy :: DoubleLinkedList a -> (a -> Bool) -> IO (Maybe a)
listRemoveBy x p = readIORef (listHead x) >>= loop
  where loop item =
          case item of
            Nothing   -> return Nothing
            Just item ->
              do let f = p (itemVal item)
                 if not f
                   then readIORef (itemNext item) >>= loop
                   else do size <- readIORef (listSize x)
                           prev <- readIORef (itemPrev item)
                           next <- readIORef (itemNext item)
                           let size' = size - 1
                           size' `seq` writeIORef (listSize x) size'
                           case (prev, next) of
                             (Nothing, Nothing) ->
                               do writeIORef (listHead x) Nothing
                                  writeIORef (listTail x) Nothing
                             (Nothing, head' @ (Just item')) ->
                               do writeIORef (itemPrev item') Nothing
                                  writeIORef (listHead x) head'
                             (tail' @ (Just item'), Nothing) ->
                               do writeIORef (itemNext item') Nothing
                                  writeIORef (listTail x) tail'
                             (Just prev', Just next') ->
                               do writeIORef (itemNext prev') (Just next')
                                  writeIORef (itemPrev next') (Just prev')
                           return (Just $ itemVal item)

-- | Detect whether the specified element is contained in the list.
listContains :: Eq a => DoubleLinkedList a -> a -> IO Bool
listContains x v = fmap isJust $ listContainsBy x (== v)

-- | Detect whether an element satisfying the specified predicate is contained in the list.
listContainsBy :: DoubleLinkedList a -> (a -> Bool) -> IO (Maybe a)
listContainsBy x p = readIORef (listHead x) >>= loop
  where loop item =
          case item of
            Nothing   -> return Nothing
            Just item ->
              do let f = p (itemVal item)
                 if not f
                   then readIORef (itemNext item) >>= loop
                   else return $ Just (itemVal item)

-- | Clear the contents of the list.
clearList :: DoubleLinkedList a -> IO ()
clearList q =
  do writeIORef (listHead q) Nothing
     writeIORef (listTail q) Nothing
     writeIORef (listSize q) 0

-- | Freeze the list and return its contents.
freezeList :: DoubleLinkedList a -> IO [a]
freezeList x = readIORef (listTail x) >>= loop []
  where loop acc Nothing     = return acc
        loop acc (Just item) = readIORef (itemPrev item) >>= loop (itemVal item : acc)
  
