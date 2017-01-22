--------------------------------------------------------------------------------
-- |
-- Module      : Poets.Conc
-- Copyright   : 3gERP, 2010
-- License     : All Rights Reserved
--
-- Maintainer  :  Patrick Bahr, Tom Hvitved
-- Stability   :  unknown
-- Portability :  unknown
--
-- Utility functions for concurrency used in POETS.
--
--------------------------------------------------------------------------------

module Poets.Conc
    (
     Lock,
     newLock,
     atomic,
     atomic_,
     RWLock,
     newRWLock,
     rwReader,
     rwWriter
    ) where

import Prelude hiding (catch)
import Control.Concurrent
import Control.Exception (throwIO, catch)
import Control.Monad
import Data.IORef

type Lock = MVar ()


newLock :: IO Lock
newLock = newMVar ()

{-|
  This function atomically executes the give IO computation
  using the given MVar as a lock.
-}

atomic :: Lock -> IO a -> IO a
atomic lock action = do
  takeMVar lock
  ret <- catch action catchErr
  putMVar lock ()
  return ret
      where catchErr :: IOError -> IO a
            catchErr err = putMVar lock () >> throwIO err

{-|
  Same as 'atomic' but discards the result of the computation.
-}

atomic_ :: Lock -> IO a -> IO ()
atomic_ lock action = do
  takeMVar lock
  catch action catchErr
  putMVar lock ()
      where catchErr :: IOError -> IO a
            catchErr err = putMVar lock () >> throwIO err

-- |A Readers-Writers lock. (Silberschatz et al., '03, Chap. 7.5.2). A
-- Readers-Writers lock allows for multiple simultaneous readers, but only one
-- writer. Example: several processes can read from the same file, but only one
-- process should write to the file at a time -- and only when there are no
-- active readers.
data RWLock = RWLock{rwLockMux :: Lock,
                     rwLockWrt :: Lock,
                     rwReadCount :: IORef Int}

-- |Instantiate a new Readers-Writers lock.
newRWLock :: IO RWLock
newRWLock = do
  mux <- newLock
  wrt <- newLock
  countR <- newIORef 0
  return RWLock{rwLockMux = mux, rwLockWrt = wrt, rwReadCount = countR}

-- |Acquire a Readers-Writers lock as a writer.
rwWriter :: RWLock -> IO a -> IO a
rwWriter RWLock{rwLockWrt = wrt} = atomic wrt

-- |Acquire a Readers-Writers lock as a reader.
rwReader :: RWLock -> IO a -> IO a
rwReader RWLock{rwLockMux = mux, rwLockWrt = wrt, rwReadCount = countR} m = do
  takeMVar mux
  count <- readIORef countR
  writeIORef countR (count + 1)
  when (count == 0) (takeMVar wrt)
  putMVar mux ()
  res <- catch m catchErr
  inc
  return res
      where inc = do
              takeMVar mux
              count <- readIORef countR
              writeIORef countR (count - 1)
              when (count == 1) (putMVar wrt ())
              putMVar mux ()
            catchErr :: IOError -> IO a
            catchErr err = do
              inc
              throwIO err