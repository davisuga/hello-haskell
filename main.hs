{-# LANGUAGE NoMonomorphismRestriction #-}

module Main where

import Cli (getGreeting)
import Control.Concurrent (forkIO)
import Data.Char (toUpper)
import Data.List (elemIndex)
import Data.Maybe (fromJust)
import FunConcurrency (funStuff, hiThread)
import FunFunctions (contains, mapAndUpper, (|>))
import System.Environment (getArgs, getProgName, lookupEnv)
import System.Exit (exitSuccess)

fib = 1 : 1 : [a + b | (a, b) <- zip fib (tail fib)]

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
  deriving (Show, Read)

instance Eq Book where
  (==) book_a book_b = isbn book_a == isbn book_b

main = do
  forkIO (getGreeting >>= print)
