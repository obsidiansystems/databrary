{-# LANGUAGE OverloadedStrings #-}

module Databrary.Controller.CitationBuilder where


import Data.Text.Encoding
import qualified Data.ByteString.Lazy          as L hiding (map)
import           Data.ByteString.Builder
import           Data.Monoid
import           Data.Foldable                 (foldMap)
import           Data.List                     (intersperse)
import           Data.ByteString.Builder
import           Databrary.Model.Volume.Types
import           Data.Time.Clock


citationBuilder :: Volume -> Builder
citationBuilder v = mconcat ["@data { \n ", "name = \"", (byteString name), "\", \n authors = \"", (byteString $ encodeUtf8 authors), "\", \n date = \"", "\" \n }"]
    where
      authors = getAuthors $ volumeOwners v
      name = mconcat $ [encodeUtf8 $ volumeName v]

-- getAuthors :: [VolumeOwner] -> Builder
getAuthors x = mconcat $ map snd x
               
-- getDate ::  UTCTime -> Builder
getDate x = mconcat x
