{-# LANGUAGE DataKinds              #-}
{-# LANGUAGE DeriveGeneric          #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiWayIf             #-}
{-# LANGUAGE OverloadedStrings      #-}
{-# LANGUAGE TypeApplications       #-}
{-# LANGUAGE TypeFamilies           #-}

module HaskellWorks.CabalCache.S3.Uri
( IsPath(..)
, Uri(..)
, toUri
)
where

import Antiope.Core                    (ToText (..))
import Antiope.S3                      (ObjectKey (..), S3Uri (..))
import Control.Lens                    hiding ((<.>))
import Data.Generics.Product.Any
import Data.Maybe                      (fromMaybe)
import Data.Text                       (Text)
import GHC.Generics                    (Generic)
import HaskellWorks.CabalCache.S3.Show
import Network.URI                     (URI)

import qualified Data.Text       as T
import qualified Network.URI     as URI
import qualified System.FilePath as FP

class IsPath a s | a -> s where
  (</>) :: a -> s -> a
  (<.>) :: a -> s -> a

infixr 5 </>
infixr 7 <.>

newtype Uri = Uri URI deriving (Show, Eq, Generic)

instance ToText Uri where
  toText (Uri uri) = tshow uri

instance IsPath Uri Text where
  Uri b </> p = Uri   (b </> p)
  Uri b <.> e = Uri   (b <.> e)

instance IsPath Text Text where
  b </> p = T.pack (T.unpack b FP.</> T.unpack p)
  b <.> e = T.pack (T.unpack b FP.<.> T.unpack e)

instance IsPath URI Text where
  b </> p = b & the @"uriPath" %~ (<> "/" <> T.unpack p)
  b <.> e = b & the @"uriPath" %~ (<> "." <> T.unpack e)

instance (a ~ Char) => IsPath [a] [a] where
  b </> p = b FP.</> p
  b <.> e = b FP.<.> e

instance IsPath S3Uri Text where
  S3Uri b (ObjectKey k) </> p =
    S3Uri b (ObjectKey (stripEnd "/" k <> "/" <> stripStart "/" p))

  S3Uri b (ObjectKey k) <.> e =
    S3Uri b (ObjectKey (stripEnd "." k <> "." <> stripStart "." e))

toUri :: Text -> Maybe Uri
toUri t = Uri <$> URI.parseURI (T.unpack t)

-------------------------------------------------------------------------------
stripStart :: Text -> Text -> Text
stripStart what txt = fromMaybe txt (T.stripPrefix what txt)

stripEnd :: Text -> Text -> Text
stripEnd what txt = fromMaybe txt (T.stripSuffix what txt)
