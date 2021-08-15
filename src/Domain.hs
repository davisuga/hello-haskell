{-# LANGUAGE DeriveGeneric #-}

module Domain where

import GHC.Generics (Generic)

data Person = Person
  { name :: String,
    profileUrl :: String
  }
  deriving (Generic, Show)