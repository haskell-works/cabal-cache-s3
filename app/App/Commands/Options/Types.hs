{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DuplicateRecordFields #-}

module App.Commands.Options.Types
  ( GetOptions(..)
  , HeadOptions(..)
  , PutOptions(..)
  , VersionOptions(..)
  ) where

import Antiope.Env  (Region)
import Data.Text    (Text)
import GHC.Generics (Generic)
import HaskellWorks.CabalCache.S3.Uri

import qualified Antiope.Env as AWS

data GetOptions = GetOptions
  { region      :: Region
  , baseUri     :: Uri
  , path        :: Text
  , configPath  :: FilePath
  , awsLogLevel :: Maybe AWS.LogLevel
  } deriving (Eq, Show, Generic)

data HeadOptions = HeadOptions
  { region      :: Region
  , baseUri     :: Uri
  , path        :: Text
  , configPath  :: FilePath
  , awsLogLevel :: Maybe AWS.LogLevel
  } deriving (Eq, Show, Generic)

data PutOptions = PutOptions
  { region      :: Region
  , baseUri     :: Uri
  , path        :: Text
  , configPath  :: FilePath
  , awsLogLevel :: Maybe AWS.LogLevel
  } deriving (Eq, Show, Generic)

data VersionOptions = VersionOptions deriving (Eq, Show, Generic)
