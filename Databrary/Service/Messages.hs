{-# LANGUAGE GeneralizedNewtypeDeriving, OverloadedStrings #-}
module Databrary.Service.Messages
  ( Messages
  , messagesFile
  , loadMessagesFrom
  , loadMessages
  , getMessage
  ) where

import Data.Maybe (fromMaybe)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE

import qualified Databrary.Store.Config as Conf
import qualified Databrary.JSON as JSON

newtype Messages = Messages Conf.Config
  deriving (JSON.ToJSON)

messagesFile :: IO FilePath
messagesFile = do
  appRoot <- Conf.get "root.path" <$> Conf.getConfig
  return (appRoot ++ "messages.conf")

loadMessagesFrom :: FilePath -> IO Messages
loadMessagesFrom f = Messages <$> Conf.load f

loadMessages :: IO Messages
loadMessages = loadMessagesFrom =<< messagesFile

getMessage :: Conf.Path -> Messages -> T.Text
getMessage p (Messages c) = fromMaybe ('[' `T.cons` TE.decodeLatin1 (Conf.pathKey p) `T.snoc` ']') $ c Conf.! p
