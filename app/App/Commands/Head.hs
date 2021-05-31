{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections       #-}
{-# LANGUAGE TypeApplications    #-}

module App.Commands.Head
  ( cmdHead
  ) where

import Antiope.Core                        (Region (..), runResAws)
import Antiope.Env                         (mkEnv)
import Antiope.Options.Applicative
import App.Commands.Options.Parser         (text)
import App.Commands.Options.Types          (HeadOptions (HeadOptions))
import Control.Applicative
import Control.Lens
import Data.Generics.Product.Any           (the)
import HaskellWorks.CabalCache.S3.IO.Lazy  (headS3Uri)
import HaskellWorks.CabalCache.S3.Uri
import Options.Applicative                 hiding (columns)

import qualified App.Commands.Options.Types         as Z
import qualified Data.Text                          as T
import qualified HaskellWorks.CabalCache.S3.AWS.Env as AWS
import qualified System.Exit                        as IO
import qualified System.IO                          as IO
import qualified System.IO.Unsafe                   as IO

{- HLINT ignore "Monoid law, left identity" -}
{- HLINT ignore "Reduce duplication"        -}
{- HLINT ignore "Redundant do"              -}

runHead :: Z.HeadOptions -> IO ()
runHead opts = do
  let awsLogLevel = opts ^. the @"awsLogLevel"
  let s3Uri       = opts ^. the @"baseUri"
  let path        = opts ^. the @"path"

  envAws <- IO.unsafeInterleaveIO $ mkEnv (opts ^. the @"region") (AWS.awsLogger awsLogLevel)
  result <- runResAws envAws $ headS3Uri envAws (s3Uri </> path)

  case result of
    Right _ -> return ()
    Left e  -> do
      IO.hPutStrLn IO.stderr $ "Error: " <> show e
      IO.exitFailure

optsHead :: Parser HeadOptions
optsHead = HeadOptions
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

cmdHead :: Mod CommandFields (IO ())
cmdHead = command "head"  $ flip info idm $ runHead <$> optsHead
