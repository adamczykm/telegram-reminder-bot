{-# LANGUAGE DeriveGeneric              #-}

module Version(apiVersion, Version) where

import           Data.Aeson
import           Data.Aeson.TH
import           GHC.Generics
import           Data.Text                (Text, pack)

newtype Version = MkVersion
  { version :: Text
  } deriving (Show, Generic)

instance ToJSON Version

apiVersion :: Version
apiVersion = MkVersion $ pack "0.1.0.0"
