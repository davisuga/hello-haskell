module Cli where

import Control.Concurrent (myThreadId)
import FunConcurrency (funStuff)
import FunFunctions (contains, mapAndUpper, (|>))
import System.Environment (getProgName, lookupEnv)
import System.Exit (exitSuccess)

determineDestination age
  | age < 14 = "go to elementary school"
  | age < 17 = "go to secondary school"
  | age < 23 = "go to university... or not"
  | otherwise = "go get a job... or go sell your art!"

greet name = putStrLn $ "Hello, " ++ name ++ "!"

askName = do
  putStrLn "What is your name?"
  getLine >>= (mapAndUpper |> greet)
  putStrLn "How old are you?"
  getLine >>= (read |> determineDestination |> putStrLn)

myShell = do
  shell <- lookupEnv "SHELL"
  putStrLn $ maybe "No shell variable found" ("Your shell is " ++) shell

showHelp = do
  getProgName
    >>= (\name -> putStrLn $ "Usage: ./" ++ name ++ " [--help | -h | --interact | -i] <name>")
  exitSuccess

showVersion = putStrLn "1.0" >> exitSuccess

determineOption :: [String] -> IO ()
determineOption [] = showHelp
determineOption args
  | args `contains` ["--help", "-h"] = showHelp
  | args `contains` ["--interact", "-i"] = askName
  | args `contains` ["--version", "-v"] = showVersion
  | args `contains` ["--fun"] = funStuff
  | otherwise = greet $ head args

getGreeting :: IO String
getGreeting = do
  -- Get id and convert to string
  tid <- myThreadId
  let greeting = "Hello from " ++ show tid
  -- Force evaluation of greeting and return
  return $! greeting