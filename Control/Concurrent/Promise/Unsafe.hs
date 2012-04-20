-- |An experimental library for lazy promises that can be evaluated in 
-- pure code.
--
-- Evaluation of a promise before its thread completes results in an 
-- indefinite block. This is accomplished by the use of 'unsafeInterleaveIO'.
-- Thus, care should be taken in using this library, since it couples the
-- execution time of pure code with an arbitrary IO computation.
-- Using System.Timeout from the timeout package can help to ensure that
-- forcing a promise is always well-defined.
--
-- For safer implementations of promises, see Control.Concurrent.Spawn from 
-- the spawn package, and Control.Concurrent.Future from the future package. 
module Control.Concurrent.Promise.Unsafe
       ( -- *Creating promises
         promise, tryPromise
         -- *Creating lists of promises
       , promises, tryPromises
       ) where

import Control.Concurrent.Thread (forkIO, Result, result)
import Control.Concurrent.Chan (Chan, newChan, readChan, writeChan)
import Control.Exception (catch)
import GHC.Conc (pseq)
import System.IO.Unsafe (unsafeInterleaveIO)

import Control.Monad
import Control.Applicative
import Prelude hiding (catch)

safePromises :: [IO a] -> IO (Chan (Result a))
safePromises ios = do
  c <- newChan
  forM_ ios $ 
    \io ->  forkIO $ (writeChan c . Right =<< io)
                     `catch` (writeChan c . Left)
  return c
                   
-- |Forks an IO computation as a thread and immediately returns 
-- a lazy future. Evaluating the future before the thread completes
-- causes it to wait for a result. If the thread halts with a thrown exception,
-- then evaluating the future will re-throw the exception.
promise :: IO a -> IO a
promise io = head <$> promises [io]

-- |Like 'promise', but does not rethrow exceptions. Instead the exception is
-- wrapped as part of the 'Result'.
tryPromise :: IO a -> IO (Result a)
tryPromise io = head <$> tryPromises [io]


-- |Forks a sequence of IO computations in multiple threads, and immediately
-- returns a list of futures. The order of the futures is determined by
-- the order in which the threads terminate. If an exception is thrown by the
-- list of threads, then the exception is re-thrown when its corresponding
-- future is evaluated.
promises :: [IO a] -> IO [a]
promises ios = do
  c <- safePromises ios
  fmap (scanl1 pseq) . forM ios $ \_ -> unsafeInterleaveIO (result =<< readChan c)
{-# NOINLINE promises #-}


-- |Like 'promises', but doesn't re-throw exceptions.
tryPromises :: [IO a] -> IO [Result a]
tryPromises ios = do
  c <- safePromises ios
  fmap (scanl1 pseq) . forM ios 
    $ \_ -> unsafeInterleaveIO (readChan c)
{-# NOINLINE tryPromises #-}