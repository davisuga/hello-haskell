{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Main where

import Data.Aeson hiding (json)
import Data.Monoid ((<>))
import Data.Text (Text, pack)
import qualified FunConcurrency
import GHC.Generics
import Web.Spock
import Web.Spock.Config

instance Semigroup Int where
  (<>) = (+)

instance Monoid Int where
  mappend = (+)
  mempty = 0

equals100 = filter (== 100)

data Artist = Artist
  { name :: String,
    artistID :: String,
    monthlyListeners :: Int
  }
  deriving (Generic, Show)

instance ToJSON Artist

instance FromJSON Artist

data Song = Song
  { title :: String,
    artists :: [Artist]
  }
  deriving (Generic, Show)

instance ToJSON Song

instance FromJSON Song

type Api = SpockM () () () ()

type ApiAction a = SpockAction () () () a

main :: IO ()
main = do
  spockCfg <- defaultSpockCfg () PCNoDatabase ()
  runSpock 8080 (spock spockCfg app)

app :: Api
app = do
  get "people" $ do
    json
      [ Artist {name = "Drake", artistID = "3TVXtAsR1Inumwj472S9r4", monthlyListeners = 51436725},
        Artist {name = "Bender", artistID = "4", monthlyListeners = 1}
      ]

  post "people" $ do
    artistData <- jsonBody' :: ApiAction Artist
    text $ pack (show artistData) <> " added"