{-# LANGUAGE NoMonomorphismRestriction #-}

module Main where

import Data.Char (toUpper)
import Data.List (elemIndex)
import Data.Maybe (fromJust)
import System.Environment (getArgs, getProgName, lookupEnv)

fib = 1 : 1 : [a + b | (a, b) <- zip fib (tail fib)]

asc :: Int -> Int -> [Int]
asc start end
  | start > end = []
  | start == end = [end]
  | start < end = start : asc (start + 1) end

asc2 :: Int -> Int -> [Int]
asc2 x y = [x .. y]

has [] _ = False
has list el
  | el == last list = True
  | otherwise = has (init list) el

contains [] _ = False
contains _ [] = False
contains list (el : elms)
  | list `has` el = True
  | otherwise = list `contains` elms

inside x xs = has xs x

uniq [] = []
uniq (x : xs)
  | xs `has` x = uniq xs
  | otherwise = x : uniq xs

isAsc [] = False
isAsc (x : xs)
  | null xs = True
  | x <= head xs = isAsc xs
  | x > head xs = False

isAsc2 [] = True
isAsc2 [x] = True
isAsc2 (x : y : xs) = x <= y && isAsc2 (y : xs)

exampleGraph = [(1, 2), (2, 3), (1, 4), (5, 3), (2, 5)]

-- iterPath (head:graph) a b compatibleA
-- hasPath :: [(Int, Int)] -> Int -> Int -> Bool
-- hasPath (head:graph) a b
--   | fst head == a =

-- Faz sua matematica maluca aqui
doMath :: [Float] -> [Float] -> [Float]
doMath a b = [1]

a >< b
  | length a /= length b = Nothing
  | otherwise = Just $ doMath a b

sum = foldr (+) 0

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

determineDestination age
  | age < 14 = "go to elementary school"
  | age < 17 = "go to secondary school"
  | age < 23 = "go to university... or not"
  | otherwise = "go get a job... or go sell your art!"

mapAndUpper = map toUpper

a |> b = b . a

greet name = putStrLn $ "Hello, " ++ name ++ "!"

askName = do
  putStrLn "What is your name?"
  getLine >>= (mapAndUpper |> greet)
  putStrLn "How old are you?"
  getLine >>= (read |> determineDestination |> putStrLn)

myShell = do
  shell <- lookupEnv "SHELL"
  putStrLn $ maybe "No shell variable found" ("Your shell is " ++) shell

showHelp =
  getProgName
    >>= (\name -> putStrLn $ "Usage: ./" ++ name ++ " [--help | -h | --interact | -i] <name>")

showVersion = putStrLn "1.0"

determineOption :: [String] -> IO ()
determineOption [] = putStrLn "Nobody to greet :/"
determineOption args
  | args `contains` ["--help", "-h"] = showHelp
  | args `contains` ["--interact", "-i"] = askName
  | otherwise = greet $ head args

log x = do
  print x
  return x

flatMap = (>>=)

main = do
  getArgs >>= Main.log >>= determineOption
