{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DuplicateRecordFields #-}

module App.Commands.Options.Types
  ( GetOptions(..)
  , HeadOptions(..)
  , PutOptions(..)
  , VersionOptions(..)
  ) where

import Antiope.Env  (Region)
import Antiope.S3   (S3Uri (..))
import Data.Text    (Text)
import GHC.Generics

import qualified Antiope.Env as AWS

data GetOptions = GetOptions
  { region      :: Region
  , baseUri     :: S3Uri
  , path        :: Text
  , awsLogLevel :: Maybe AWS.LogLevel
  } deriving (Eq, Show, Generic)

data HeadOptions = HeadOptions
  { region      :: Region
  , baseUri     :: S3Uri
  , path        :: Text
  , awsLogLevel :: Maybe AWS.LogLevel
  } deriving (Eq, Show, Generic)

data PutOptions = PutOptions
  { region      :: Region
  , baseUri     :: S3Uri
  , path        :: Text
  , awsLogLevel :: Maybe AWS.LogLevel
  } deriving (Eq, Show, Generic)

data VersionOptions = VersionOptions deriving (Eq, Show, Generic)
