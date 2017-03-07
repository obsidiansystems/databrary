{-# LANGUAGE OverloadedStrings #-}
module Databrary.Web.Uglify
  ( appWebJS
  , generateUglifyJS
  ) where

import Control.Monad (guard, liftM2)
import Control.Monad.IO.Class (liftIO)
import Data.List (isPrefixOf, union)
import qualified System.FilePath as FP
import System.Process (callProcess)

import Databrary.Files
import Databrary.Web
import Databrary.Web.Types
import Databrary.Web.Files
import Databrary.Web.Generate
import Databrary.Web.Libs
import qualified Databrary.Store.Config as Conf

appWebJS :: IO [WebFilePath]
appWebJS = liftM2 union
  ((webIncludes ++) . (tail pre ++) .
    filter (\f -> not (isPrefixOf "lib/" (webFileRel f)) && f `notElem` pre) <$> findWebFiles ".js")
  (map (replaceWebExtension ".js") <$> findWebFiles ".coffee") where
  pre = ["debug.js", "app.js", "constants.js", "routes.js", "messages.js", "templates.js"]

generateUglifyJS :: WebGenerator
generateUglifyJS fo@(f, _) = do
  jl <- liftIO appWebJS
  guard (not $ null jl)
  nodeModulesPath <- liftIO $ Conf.get "node.modules.path" <$> Conf.getConfig
  let fm = f <.> ".map"
      uglifyArgs = [ "--output"
                   , webFileAbs f
                   , "--source-map"
                   , webFileAbs fm
                   , "--source-map-url"
                   , webFileRel fm
                   , "--prefix"
                   , "relative"
                   , "--screw-ie8"
                   , "--mangle"
                   , "--compress"
                   , "--define"
                   , "DEBUG=false"
                   , "--wrap"
                   , "app"]
      uglifyBinPath = nodeModulesPath FP.</> ".bin" FP.</> "uglifyjs"
  webRegenerate (callProcess uglifyBinPath $ uglifyArgs ++ map webFileAbs jl) [] jl fo
