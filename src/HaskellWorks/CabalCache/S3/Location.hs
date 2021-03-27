{-# LANGUAGE DeriveGeneric          #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiWayIf             #-}
{-# LANGUAGE OverloadedStrings      #-}
{-# LANGUAGE TypeFamilies           #-}

module HaskellWorks.CabalCache.S3.Location
  ( IsPath(..)
  , toS3Uri
  ) where

import Antiope.Core (fromText)
import Antiope.S3   (ObjectKey (..), S3Uri (..))
import Data.Maybe   (fromMaybe)
import Data.Text    (Text)

import qualified Data.Text       as T
import qualified System.FilePath as FP

class IsPath a s | a -> s where
  (</>) :: a -> s -> a
  (<.>) :: a -> s -> a

infixr 5 </>
infixr 7 <.>

instance IsPath Text Text where
  b </> p = T.pack (T.unpack b FP.</> T.unpack p)
  b <.> e = T.pack (T.unpack b FP.<.> T.unpack e)

instance (a ~ Char) => IsPath [a] [a] where
  b </> p = b FP.</> p
  b <.> e = b FP.<.> e

instance IsPath S3Uri Text where
  S3Uri b (ObjectKey k) </> p =
    S3Uri b (ObjectKey (stripEnd "/" k <> "/" <> stripStart "/" p))

  S3Uri b (ObjectKey k) <.> e =
    S3Uri b (ObjectKey (stripEnd "." k <> "." <> stripStart "." e))

toS3Uri :: Text -> Maybe S3Uri
toS3Uri txt = either (const Nothing) Just (fromText txt')
  where txt' = T.strip txt

-------------------------------------------------------------------------------
stripStart :: Text -> Text -> Text
stripStart what txt = fromMaybe txt (T.stripPrefix what txt)

stripEnd :: Text -> Text -> Text
stripEnd what txt = fromMaybe txt (T.stripSuffix what txt)
