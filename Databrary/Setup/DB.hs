{-# LANGUAGE OverloadedStrings #-}
module Main (main) where

import System.Environment (getArgs)
import qualified System.Console.GetOpt as Opt

import Data.Either (partitionEithers)
import Databrary.Service.DB (runDBConnection)
import Databrary.Service.DB.Schema
import qualified Databrary.Store.Config as Conf

data Flag = FlagConfig FilePath
  deriving (Eq)

opts :: [Opt.OptDescr Flag]
opts =
  [ Opt.Option "c" ["config"] (Opt.ReqArg FlagConfig "FILE") "Path to configuration file"]

flagConfig :: Flag -> Either FilePath Flag
flagConfig (FlagConfig f) = Left f
flagConfig f = Right f

main :: IO ()
main = do
  args <- getArgs
  let (flags, _, _) = Opt.getOpt Opt.Permute opts args
      (configPaths, _) = partitionEithers $ map flagConfig flags
  conf <- Conf.initConfig configPaths
  let schemapath = Conf.get "db.schemapath" conf
  let unattendedUpdate = True
  runDBConnection $ updateDBSchema schemapath unattendedUpdate
