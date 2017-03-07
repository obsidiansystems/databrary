{-# LANGUAGE OverloadedStrings #-}
module Databrary.Web.Coffee
  ( generateCoffeeJS
  ) where

import Control.Monad (mzero)
import System.FilePath (takeDirectory)
import System.Process (callProcess)
import Control.Monad.IO.Class (liftIO)

import Databrary.Files
import Databrary.Web
import Databrary.Web.Types
import Databrary.Web.Generate
import qualified Databrary.Store.Config as Conf

generateCoffeeJS :: WebGenerator
generateCoffeeJS fo@(f, _)
  | (b, e) <- splitWebExtensions f, e `elem` [".js", ".js.map"] = do
    let src = b <.> ".coffee"
    nodeModulesPath <- liftIO $ Conf.get "node.modules.path" <$> Conf.getConfig
    let coffeeBinPath = nodeModulesPath </> ".bin" </> "coffee"
        coffeeArgs = ["-b", "-c", "-m", "-o", takeDirectory (webFileAbs f), webFileAbs src ]
    webRegenerate (callProcess coffeeBinPath coffeeArgs) [] [src] fo
  | otherwise = mzero
