{-# LANGUAGE CPP, OverloadedStrings #-}
module Databrary.Warp
  ( runWarp
  ) where

import Control.Applicative ((<|>))
import qualified Data.ByteString.Char8 as BSC
import Data.Monoid ((<>))
import Data.Time (getCurrentTime)
import Data.Version (showVersion)
import qualified Network.Wai as Wai
import qualified Network.Wai.Handler.Warp as Warp
import qualified Network.Wai.Handler.WarpTLS as WarpTLS

import Paths_databrary (version)
import qualified Databrary.Store.Config as Conf
import Databrary.Service.Types
import Databrary.Service.Log

runWarp :: Conf.Config -> Service -> Wai.Application -> IO ()
runWarp conf rc app =
  run (conf Conf.! "ssl.key") (certs $ conf Conf.! "ssl.cert")
    ( Warp.setPort (conf Conf.! "port")
    $ Warp.setTimeout 300
#ifndef DEVEL
    $ Warp.setFdCacheDuration 300
    $ Warp.setFileInfoCacheDuration 300
#endif
    $ Warp.setServerName (BSC.pack $ "databrary/" ++ showVersion version)
    $ Warp.setOnException (\req e -> do
      t <- getCurrentTime
      msg <- mapM (\q -> requestLog t q Nothing $ Warp.exceptionResponseForDebug e) req
      logMsg t (maybe id (\m -> (<>) (m <> "\n")) msg $ toLogStr $ show e) (serviceLogs rc))
    $ Warp.setHTTP2Disabled
    $ Warp.defaultSettings)
    app
  where
  certs c = Conf.config c <|> return <$> Conf.config c
  run (Just k) (Just (cert:chain)) = WarpTLS.runTLS (WarpTLS.tlsSettingsChain cert chain k)
  run _ _ = Warp.runSettings
