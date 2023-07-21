{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ScopedTypeVariables        #-}

-- |
-- Module      : Network.Google.Data.JSON
-- Copyright   : (c) 2015-2016 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : provisional
-- Portability : non-portable (GHC extensions)
--
module Network.Google.Data.JSON
    ( JSONValue
    , Textual  (..)
    , parseJSONObject
    , parseJSONText
    , toJSONText

    -- * Re-exports
    , Aeson.FromJSON (..)
    , Aeson.ToJSON   (..)

    , Aeson.withObject
    , Aeson.emptyObject
    , Aeson.object

    , (Aeson..=)
    , (Aeson..:)
    , (Aeson..:?)
    , (Aeson..!=)
    ) where

import           qualified Data.Aeson as Aeson
import           qualified Data.Aeson.Types as Aeson
import           Data.Data
import           Data.HashMap.Strict (HashMap)
import           Data.Text           (Text)
import qualified Data.Text           as Text
import           Web.HttpApiData     (FromHttpApiData (..), ToHttpApiData (..))

type JSONValue = Aeson.Value

newtype Textual a = Textual a
    deriving
        ( Eq
        , Ord
        , Read
        , Show
        , Num
        , Fractional
        , Data
        , Typeable
        , ToHttpApiData
        , FromHttpApiData
        )

instance (Aeson.FromJSON a, FromHttpApiData a) => Aeson.FromJSON (Textual a) where
    parseJSON (Aeson.String s) =
        either (fail . Text.unpack) (pure . Textual) (parseQueryParam s)
    parseJSON o          = Textual <$> Aeson.parseJSON o

instance ToHttpApiData a => Aeson.ToJSON (Textual a) where
    toJSON (Textual x) = Aeson.String (toQueryParam x)

parseJSONObject :: Aeson.FromJSON a => Aeson.Object -> Aeson.Parser a
parseJSONObject = Aeson.parseJSON . Aeson.Object

parseJSONText :: FromHttpApiData a => String -> Aeson.Value -> Aeson.Parser a
parseJSONText n = Aeson.withText n (either (fail . f) pure . parseQueryParam)
  where
    f x = n ++ " - " ++ Text.unpack x

toJSONText :: ToHttpApiData a => a -> Aeson.Value
toJSONText = Aeson.String . toQueryParam
