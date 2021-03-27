{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}

module HaskellWorks.CabalCache.S3.IO.Lazy
  ( headS3Uri
  , getS3Uri
  , putS3Uri
  ) where

import Antiope.Core
import Antiope.S3.Lazy                     (S3Uri)
import Control.Lens
import Control.Monad.Catch
import Control.Monad.Except
import Control.Monad.Trans.Resource
import Data.Generics.Product.Any
import HaskellWorks.CabalCache.S3.AppError

import qualified Antiope.S3.Lazy           as AWS
import qualified Data.ByteString.Lazy      as LBS
import qualified Data.Text                 as T
import qualified Network.AWS               as AWS
import qualified Network.AWS.S3.HeadObject as AWS
import qualified Network.AWS.S3.PutObject  as AWS
import qualified Network.HTTP.Types        as HTTP

{- HLINT ignore "Redundant do"        -}
{- HLINT ignore "Reduce duplication"  -}
{- HLINT ignore "Redundant bracket"   -}

handleAwsError :: MonadCatch m => m a -> m (Either AppError a)
handleAwsError f = catch (Right <$> f) $ \(e :: AWS.Error) ->
  case e of
    (AWS.ServiceError (AWS.ServiceError' _ s@(HTTP.Status 404 _) _ _ _ _)) -> return (Left (AwsAppError s))
    (AWS.ServiceError (AWS.ServiceError' _ s@(HTTP.Status 301 _) _ _ _ _)) -> return (Left (AwsAppError s))
    _                                                                      -> throwM e

getS3Uri :: (MonadResource m, MonadCatch m) => AWS.Env -> AWS.S3Uri -> m (Either AppError LBS.ByteString)
getS3Uri envAws s3Uri = case reslashS3Uri s3Uri of
  (AWS.S3Uri b k) -> handleAwsError $ runAws envAws $ AWS.unsafeDownload b k

headS3Uri :: (MonadResource m, MonadCatch m) => AWS.Env -> AWS.S3Uri -> m (Either AppError AWS.HeadObjectResponse)
headS3Uri envAws s3Uri = case reslashS3Uri s3Uri of
  AWS.S3Uri b k -> handleAwsError $ runAws envAws $ AWS.send $ AWS.headObject b k

putS3Uri :: (MonadUnliftIO m, MonadCatch m) => AWS.Env -> AWS.S3Uri -> LBS.ByteString -> m (Either AppError ())
putS3Uri envAws s3Uri lbs = case reslashS3Uri s3Uri of
  AWS.S3Uri b k -> do
    let req = AWS.toBody lbs
    let po  = AWS.putObject b k req
    handleAwsError $ void $ runResAws envAws $ AWS.send po

reslashS3Uri :: S3Uri -> S3Uri
reslashS3Uri uri = uri & the @"objectKey" . the @1 %~ T.replace "\\" "/"
