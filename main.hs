import Data.List
import System.IO

class Matcher a where
  matches :: a -> a -> Bool

class MyEq a where
  areEqual :: a -> a -> Bool

data ShirtSize = S | M | L

instance MyEq ShirtSize where
  areEqual S S = True
  areEqual M M = True
  areEqual L L = True
  areEqual _ _ = False

data User = User
  { name :: String,
    email :: String,
    age :: Integer,
    interests :: [String]
  }
  deriving (Show)

peter =
  User
    { name = "Pedro Cleto",
      email = "cl3t0.sh@gmail.com",
      age = 19,
      interests = ["Math", "Computers", "One Piece", "Competitive Games"]
    }

dave =
  User
    { name = "Davi",
      email = "daviciencia1@gmail.com",
      age = 18,
      interests = ["Action Games", "Computers", "One Piece", "The Boys"]
    }
