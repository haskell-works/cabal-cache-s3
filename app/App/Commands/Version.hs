{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}

module App.Commands.Version
  ( cmdVersion
  ) where

import App.Commands.Options.Parser (optsVersion)
import Data.List
import Options.Applicative         hiding (columns)

import qualified App.Commands.Options.Types            as Z
import qualified Data.Text                             as T
import qualified Data.Version                          as V
import qualified HaskellWorks.CabalCache.S3.IO.Console as CIO
import qualified Paths_cabal_cache_s3                  as P

{- HLINT ignore "Redundant do"        -}
{- HLINT ignore "Reduce duplication"  -}

runVersion :: Z.VersionOptions -> IO ()
runVersion _ = do
  let V.Version {..} = P.version

  let version = intercalate "." $ fmap show versionBranch

  CIO.putStrLn $ "cabal-cache-s3 " <> T.pack version

cmdVersion :: Mod CommandFields (IO ())
cmdVersion = command "version"  $ flip info idm $ runVersion <$> optsVersion
