
-- |
-- Module     : Simulation.Aivika.Dynamics.Internal.Cont
-- Copyright  : Copyright (c) 2009-2013, David Sorokin <david.sorokin@gmail.com>
-- License    : BSD3
-- Maintainer : David Sorokin <david.sorokin@gmail.com>
-- Stability  : experimental
-- Tested with: GHC 7.6.3
--
-- The 'Cont' monad is a variation of the standard Cont monad 
-- and F# async workflow, where the result of applying 
-- the continuation is a dynamic process.
--
module Simulation.Aivika.Dynamics.Internal.Cont
       (Cont(..),
        ContParams,
        runCont,
        catchCont,
        finallyCont,
        throwCont,
        resumeContByParams,
        contParamsCanceled) where

import Data.IORef

import qualified Control.Exception as C
import Control.Exception (IOException, throw)

import Control.Monad
import Control.Monad.Trans

import Simulation.Aivika.Dynamics.Internal.Simulation
import Simulation.Aivika.Dynamics.Internal.Dynamics

-- | The 'Cont' type is similar to the standard Cont monad 
-- and F# async workflow but only the continuations return
-- a dynamic process as a result.
newtype Cont a = Cont (ContParams a -> Dynamics ())

-- | The continuation parameters.
data ContParams a = 
  ContParams { contCont :: a -> Dynamics (), 
               contAux  :: ContParamsAux }

-- | The auxiliary continuation parameters.
data ContParamsAux =
  ContParamsAux { contECont :: IOException -> Dynamics (),
                  contCCont :: () -> Dynamics (),
                  contCancelRef :: IORef Bool, 
                  contCatchFlag :: Bool }

instance Monad Cont where
  return  = returnC
  m >>= k = bindC m k

instance SimulationLift Cont where
  liftSimulation = liftSC

instance DynamicsLift Cont where
  liftDynamics = liftDC

instance Functor Cont where
  fmap = liftM

instance MonadIO Cont where
  liftIO = liftIOC 

invokeC :: Cont a -> ContParams a -> Dynamics ()
{-# INLINE invokeC #-}
invokeC (Cont m) = m

invokeD :: Point -> Dynamics a -> IO a
{-# INLINE invokeD #-}
invokeD p (Dynamics m) = m p

cancelD :: Point -> ContParams a -> IO ()
{-# NOINLINE cancelD #-}
cancelD p c =
  do writeIORef (contCancelRef . contAux $ c) False
     invokeD p $ (contCCont . contAux $ c) ()

returnC :: a -> Cont a
{-# INLINE returnC #-}
returnC a = 
  Cont $ \c ->
  Dynamics $ \p ->
  do z <- readIORef $ (contCancelRef . contAux) c
     if z 
       then cancelD p c
       else invokeD p $ contCont c a
                          
-- bindC :: Cont a -> (a -> Cont b) -> Cont b
-- {-# INLINE bindC #-}
-- bindC m k = 
--   Cont $ \c -> 
--   if (contCatchFlag . contAux $ c) 
--   then bindWithCatch m k c
--   else bindWithoutCatch m k c
  
bindC :: Cont a -> (a -> Cont b) -> Cont b
{-# INLINE bindC #-}
bindC m k = 
  Cont $ bindWithoutCatch m k  -- Another version is not tail recursive!
  
bindWithoutCatch :: Cont a -> (a -> Cont b) -> ContParams b -> Dynamics ()
{-# INLINE bindWithoutCatch #-}
bindWithoutCatch (Cont m) k c = 
  Dynamics $ \p ->
  do z <- readIORef $ (contCancelRef . contAux) c
     if z 
       then cancelD p c
       else invokeD p $ m $ 
            let cont a = invokeC (k a) c
            in c { contCont = cont }

-- It is not tail recursive!
bindWithCatch :: Cont a -> (a -> Cont b) -> ContParams b -> Dynamics ()
{-# NOINLINE bindWithCatch #-}
bindWithCatch (Cont m) k c = 
  Dynamics $ \p ->
  do z <- readIORef $ (contCancelRef . contAux) c
     if z 
       then cancelD p c
       else invokeD p $ m $ 
            let cont a = catchDynamics 
                         (invokeC (k a) c)
                         (contECont . contAux $ c)
            in c { contCont = cont }

-- Like "bindWithoutCatch (return a) k"
callWithoutCatch :: (a -> Cont b) -> a -> ContParams b -> Dynamics ()
callWithoutCatch k a c =
  Dynamics $ \p ->
  do z <- readIORef $ (contCancelRef . contAux) c
     if z 
       then cancelD p c
       else invokeD p $ invokeC (k a) c

-- Like "bindWithCatch (return a) k" but it is not tail recursive!
callWithCatch :: (a -> Cont b) -> a -> ContParams b -> Dynamics ()
callWithCatch k a c =
  Dynamics $ \p ->
  do z <- readIORef $ (contCancelRef . contAux) c
     if z 
       then cancelD p c
       else invokeD p $ catchDynamics 
            (invokeC (k a) c)
            (contECont . contAux $ c)

-- | Exception handling within 'Cont' computations.
catchCont :: Cont a -> (IOException -> Cont a) -> Cont a
catchCont m h = 
  Cont $ \c -> 
  if contCatchFlag . contAux $ c
  then catchWithCatch m h c
  else error $
       "To catch exceptions, the process must be created " ++
       "with help of newProcessIDWithCatch: catchCont."
  
catchWithCatch :: Cont a -> (IOException -> Cont a) -> ContParams a -> Dynamics ()
catchWithCatch (Cont m) h c =
  Dynamics $ \p -> 
  do z <- readIORef $ (contCancelRef . contAux) c
     if z 
       then cancelD p c
       else invokeD p $ m $
            -- let econt e = callWithCatch h e c   -- not tail recursive!
            let econt e = callWithoutCatch h e c
            in c { contAux = (contAux c) { contECont = econt } }
               
-- | A computation with finalization part.
finallyCont :: Cont a -> Cont b -> Cont a
finallyCont m m' = 
  Cont $ \c -> 
  if contCatchFlag . contAux $ c
  then finallyWithCatch m m' c
  else error $
       "To finalize computation, the process must be created " ++
       "with help of newProcessIDWithCatch: finallyCont."
  
finallyWithCatch :: Cont a -> Cont b -> ContParams a -> Dynamics ()               
finallyWithCatch (Cont m) (Cont m') c =
  Dynamics $ \p ->
  do z <- readIORef $ (contCancelRef . contAux) c
     if z 
       then cancelD p c
       else invokeD p $ m $
            let cont a   = 
                  Dynamics $ \p ->
                  invokeD p $ m' $
                  let cont b = contCont c a
                  in c { contCont = cont }
                econt e  =
                  Dynamics $ \p ->
                  invokeD p $ m' $
                  let cont b = (contECont . contAux $ c) e
                  in c { contCont = cont }
                ccont () = 
                  Dynamics $ \p ->
                  invokeD p $ m' $
                  let cont b  = (contCCont . contAux $ c) ()
                      econt e = (contCCont . contAux $ c) ()
                  in c { contCont = cont,
                         contAux  = (contAux c) { contECont = econt } }
            in c { contCont = cont,
                   contAux  = (contAux c) { contECont = econt,
                                            contCCont = ccont } }

-- | Throw the exception with the further exception handling.
-- By some reasons, the standard 'throw' function per se is not handled 
-- properly within 'Cont' computations, altough it will be still handled 
-- if it will be hidden under the 'liftIO' function. The problem arises 
-- namely with the @throw@ function, not 'IO' computations.
throwCont :: IOException -> Cont a
throwCont e = liftIO $ throw e

-- | Run the 'Cont' computation with the specified cancelation token 
-- and flag indicating whether to catch exceptions.
runCont :: Cont a -> 
           (a -> Dynamics ()) ->
           (IOError -> Dynamics ()) ->
           (() -> Dynamics ()) ->
           IORef Bool -> 
           Bool -> 
           Dynamics ()
runCont (Cont m) cont econt ccont cancelToken catchFlag = 
  m ContParams { contCont = cont,
                 contAux  = 
                   ContParamsAux { contECont = econt,
                                   contCCont = ccont,
                                   contCancelRef = cancelToken, 
                                   contCatchFlag = catchFlag } }

-- | Lift the 'Simulation' computation.
liftSC :: Simulation a -> Cont a
liftSC (Simulation m) = 
  Cont $ \c ->
  Dynamics $ \p ->
  if contCatchFlag . contAux $ c
  then liftIOWithCatch (m $ pointRun p) p c
  else liftIOWithoutCatch (m $ pointRun p) p c
     
-- | Lift the 'Dynamics' computation.
liftDC :: Dynamics a -> Cont a
liftDC (Dynamics m) =
  Cont $ \c ->
  Dynamics $ \p ->
  if contCatchFlag . contAux $ c
  then liftIOWithCatch (m p) p c
  else liftIOWithoutCatch (m p) p c
     
-- | Lift the IO computation.
liftIOC :: IO a -> Cont a
liftIOC m =
  Cont $ \c ->
  Dynamics $ \p ->
  if contCatchFlag . contAux $ c
  then liftIOWithCatch m p c
  else liftIOWithoutCatch m p c
  
liftIOWithoutCatch :: IO a -> Point -> ContParams a -> IO ()
{-# INLINE liftIOWithoutCatch #-}
liftIOWithoutCatch m p c =
  do z <- readIORef $ (contCancelRef . contAux) c
     if z
       then cancelD p c
       else do a <- m
               invokeD p $ contCont c a

liftIOWithCatch :: IO a -> Point -> ContParams a -> IO ()
{-# NOINLINE liftIOWithCatch #-}
liftIOWithCatch m p c =
  do z <- readIORef $ (contCancelRef . contAux) c
     if z
       then cancelD p c
       else do aref <- newIORef undefined
               eref <- newIORef Nothing
               C.catch (m >>= writeIORef aref) 
                 (writeIORef eref . Just)
               e <- readIORef eref
               case e of
                 Nothing -> 
                   do a <- readIORef aref
                      -- tail recursive
                      invokeD p $ contCont c a
                 Just e ->
                   -- tail recursive
                   invokeD p $ (contECont . contAux) c e

-- | Resume the computation by the specified parameters.
resumeContByParams :: ContParams a -> a -> Dynamics ()
{-# INLINE resumeContByParams #-}
resumeContByParams c a = 
  Dynamics $ \p ->
  do z <- readIORef $ (contCancelRef . contAux) c
     if z
       then cancelD p c
       else invokeD p $ contCont c a

-- | Test whether the computation is canceled
contParamsCanceled :: ContParams a -> IO Bool
{-# INLINE contParamsCanceled #-}
contParamsCanceled c = 
  readIORef $ (contCancelRef . contAux) c
