{-# LANGUAGE OverloadedStrings #-}
module Databrary.Action.Request
  ( RequestM
  , Request
  , getRequestHeader
  , getRequestHeaders
  , isApi
  ) where

import qualified Data.ByteString as BS
import Network.HTTP.Types (HeaderName)
import Network.Wai (Request, requestHeaders, pathInfo)

import Control.Has (MonadHas, peeks)

type RequestM c m = MonadHas Request c m

getRequestHeader :: RequestM c m => HeaderName -> m (Maybe BS.ByteString)
getRequestHeader h = peeks $ lookup h . requestHeaders

getRequestHeaders :: RequestM c m => HeaderName -> m [BS.ByteString]
getRequestHeaders h = peeks $ map snd . filter ((h ==) . fst) . requestHeaders

isApi :: RequestM c m => m Bool
isApi = peeks (isapi . pathInfo) where
  isapi ("api":_) = True
  isapi _ = False
