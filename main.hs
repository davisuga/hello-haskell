import Data.List
import System.IO

sayHi = do
  putStrLn "Whats your name?"
  name <- getLine
  putStrLn $ "Hi " ++ name

writeToFile = do
  file <- openFile "test.txt" WriteMode
  hPutStrLn file "random line of text"
  hClose file

readFromFile = do
  file2 <- openFile "test.txt" ReadMode
  fileContent <- hGetContents file2
  putStr fileContent
  hClose file2