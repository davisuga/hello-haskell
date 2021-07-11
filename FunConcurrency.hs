module FunConcurrency where

import Control.Concurrent
import GHC.MVar

doSum :: Num a => a -> a -> GHC.MVar.MVar a -> IO ()
doSum a b mVar =
  putMVar mVar (a + b)

threadedSum :: (Num a, Show a) => [a] -> IO a
threadedSum list = do
  let result = sum list
  print result
  return result

hiThread :: IO ()
hiThread = do
  tid <- myThreadId
  result <- threadedSum [1 .. 20000000]
  print result
  print tid

funStuff :: IO ()
funStuff = do
  count <- newEmptyMVar
  forkIO $ doSum 10000000000 2000000013412341234000 count
  result <- takeMVar count
  print result