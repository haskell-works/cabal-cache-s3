{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections       #-}
{-# LANGUAGE TypeApplications    #-}

module App.Commands.Get
  ( cmdGet
  ) where

import Antiope.Core                        (Region (..), runResAws)
import Antiope.Env                         (mkEnv)
import Antiope.Options.Applicative
import App.Commands.Options.Parser         (text)
import App.Commands.Options.Types          (GetOptions (GetOptions))
import Control.Applicative
import Control.Lens
import Control.Monad.IO.Class
import Data.Generics.Product.Any           (the)
import HaskellWorks.CabalCache.S3.IO.Lazy  (getS3Uri)
import HaskellWorks.CabalCache.S3.Location (toS3Uri)
import HaskellWorks.CabalCache.S3.Uri
import Network.AWS.Data                    (toText)
import Options.Applicative                 hiding (columns)

import qualified App.Commands.Options.Types         as Z
import qualified Data.ByteString.Lazy               as LBS
import qualified Data.Text                          as T
import qualified Data.Text.IO                       as T
import qualified HaskellWorks.CabalCache.S3.AWS.Env as AWS
import qualified System.Exit                        as IO
import qualified System.IO                          as IO
import qualified System.IO.Unsafe                   as IO

{- HLINT ignore "Monoid law, left identity" -}
{- HLINT ignore "Reduce duplication"        -}
{- HLINT ignore "Redundant do"              -}

runGet :: Z.GetOptions -> IO ()
runGet opts = do
  let awsLogLevel = opts ^. the @"awsLogLevel"
  let s3Uri       = opts ^. the @"baseUri"
  let path        = opts ^. the @"path"
  let configPath  = opts ^. the @"configPath"

  T.hPutStrLn IO.stderr $ "Location: " <> toText (s3Uri </> path)

  envAws <- IO.unsafeInterleaveIO $ mkEnv (opts ^. the @"region") (AWS.awsLogger awsLogLevel)
  runResAws envAws $ do
    result <- getS3Uri envAws (s3Uri </> path)

    case result of
      Right contents -> liftIO $ LBS.hPut IO.stdout contents
      Left e         -> do
        liftIO $ IO.hPutStrLn IO.stderr $ "Error: " <> show e
        liftIO IO.exitFailure

optsGet :: Parser GetOptions
optsGet = GetOptions
  <$> option (auto <|> text)
      (  long "region"
      <> metavar "AWS_REGION"
      <> showDefault <> value Oregon
      <> help "The AWS region in which to operate"
      )
  <*> option (maybeReader (toUri . T.pack))
      (   long "uri"
      <>  help "Base URI to sync to"
      <>  metavar "S3_URI"
      )
  <*> strOption
      (   long "sub-key"
      <>  help "Sub key for stored object"
      <>  metavar "DIRECTORY"
      )
  <*> strOption
      (   long "config-file"
      <>  help "Sub key for stored object"
      <>  metavar "DIRECTORY"
      )
  <*> optional
      ( option autoText
        (   long "aws-log-level"
        <>  help "AWS Log Level.  One of (Error, Info, Debug, Trace)"
        <>  metavar "AWS_LOG_LEVEL"
        )
      )

cmdGet :: Mod CommandFields (IO ())
cmdGet = command "get"  $ flip info idm $ runGet <$> optsGet
