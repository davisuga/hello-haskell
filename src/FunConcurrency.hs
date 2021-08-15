module FunConcurrency where

import Control.Concurrent
  ( forkIO,
    myThreadId,
    newEmptyMVar,
    putMVar,
    takeMVar,
  )
import FunFunctions (fib)
import GHC.MVar (MVar)

doSum a b mVar =
  putMVar mVar (a + b)

threadedSum list = do
  let result = sum list
  print result
  return result

hiThread = do
  tid <- myThreadId
  result <- threadedSum [1 .. 20000000]
  print result
  print tid

funStuff = do
  count <- newEmptyMVar
  forkIO $ doSum 10000000000 2000000013412341234000 count
  result <- takeMVar count
  print result

calcFibMvar n = do
  result <- newEmptyMVar
  putMVar result (fib n)
  return result
