{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE NoMonomorphismRestriction #-}

module FunTypes where

import Cli (getGreeting)
import Control.Concurrent (forkIO, takeMVar)
import Data.Char (toUpper)
import Data.List (elemIndex)
import Data.Maybe (fromJust)
import FunConcurrency
import FunFunctions (contains, mapAndUpper, (|>))
import GHC.Records (getField)
import System.Environment (getArgs, getProgName, lookupEnv)
import System.Exit (exitSuccess)

data Author = Author
  { name :: String,
    bio :: String
  }
  deriving (Show, Read)

data Book = Book
  { title :: String,
    isbn :: Int,
    author :: String
  }

compareIsbn book_a book_b = isbn book_a == isbn book_a

instance Eq Book where
  (==) = compareIsbn

class FunFunctor f where
  fmap :: (a -> b) -> f a -> f b

-- compareIsbn :: Book -> Book -> Bool

-- compareIsbn = map $ (==) . isbn
-- compareIsbn :: Book -> Int -> Bool
-- compareIsbn = (==) . isbn



main = do
  let b = Book "Title" 100 "Dave"
  let bookAuthor = getField @"author" b

  forkIO (getGreeting >>= print)
  print "calculating fib of 100..."
  calcFibMvar 100 >>= takeMVar >>= print
  print "calculating fib of 1000..."
  calcFibMvar 1000 >>= takeMVar >>= print
  print "calculating fib of 1000000..."
  calcFibMvar 1000000 >>= takeMVar >>= print
