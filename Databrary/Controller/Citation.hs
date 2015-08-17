{-# LANGUAGE OverloadedStrings #-}
module Databrary.Controller.Citation
  ( getCitation
  ) where

import Data.Aeson (toJSON)
import qualified Data.ByteString.Lazy as BS
import qualified Data.ByteString.Builder as BSB
import Databrary.Has (focusIO)
import Databrary.Action
import Databrary.HTTP.Form.Deform
import Databrary.HTTP.Path.Parser
import Databrary.Controller.Form
import Databrary.Model.Volume.Types
import Databrary.Controller.CitationBuilder as CiteBuilder
import Databrary.Model.Citation.CrossRef

getCitation :: AppRoute ()
getCitation = action GET (pathJSON </< "cite") $ \() -> do
  url <- runForm Nothing $ "url" .:> deform
  cite <- maybeAction =<< focusIO (lookupCitation url)
  okResponse [] $ toJSON cite


getBibTeX :: AppRoute ()
getBibTeX = action GET (pathJSON </< "volume") $ \() -> do
  let v =  blankVolume
--  url <- runForm Nothing $ "url" .:>deform
--  cite <- maybeAction =<< focusIO (lookupCitation url)
  okResponse [] $ CiteBuilder.citationBuilder v

-- getRIS :: AppRoute ()
-- getRIS = action GET (pathJSON </< "cite") $ \() -> do
--   url <- runForm Nothing $ "url" .:>deform
--   cite <- maybeAction =<< focusIO (lookupCitation url)
--   okResponse [] $ CiteBuilder.risBuilder cite

