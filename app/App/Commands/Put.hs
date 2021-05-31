{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections       #-}
{-# LANGUAGE TypeApplications    #-}

module App.Commands.Put
  ( cmdPut
  ) where

import Antiope.Core                        (Region (..), runResAws)
import Antiope.Env                         (mkEnv)
import Antiope.Options.Applicative
import App.Commands.Options.Parser         (text)
import App.Commands.Options.Types          (PutOptions (PutOptions))
import Control.Applicative
import Control.Lens
import Data.Generics.Product.Any           (the)
import HaskellWorks.CabalCache.S3.IO.Lazy  (putS3Uri)
import HaskellWorks.CabalCache.S3.Uri
import Options.Applicative                 hiding (columns)

import qualified App.Commands.Options.Types         as Z
import qualified Data.ByteString.Lazy               as LBS
import qualified Data.Text                          as T
import qualified HaskellWorks.CabalCache.S3.AWS.Env as AWS
import qualified System.Exit                        as IO
import qualified System.IO                          as IO

{- HLINT ignore "Monoid law, left identity" -}
{- HLINT ignore "Reduce duplication"        -}
{- HLINT ignore "Redundant do"              -}

runPut :: Z.PutOptions -> IO ()
runPut opts = do
  let awsLogLevel = opts ^. the @"awsLogLevel"
  let s3Uri       = opts ^. the @"baseUri"
  let path        = opts ^. the @"path"

  envAws    <- mkEnv (opts ^. the @"region") (AWS.awsLogger awsLogLevel)
  contents  <- LBS.hGetContents IO.stdin
  result    <- runResAws envAws $ putS3Uri envAws (s3Uri </> path) contents

  case result of
    Right _ -> return ()
    Left e  -> do
      IO.hPutStrLn IO.stderr $ "Error: " <> show e
      IO.exitFailure

optsPut :: Parser PutOptions
optsPut = PutOptions
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

cmdPut :: Mod CommandFields (IO ())
cmdPut = command "put"  $ flip info idm $ runPut <$> optsPut
