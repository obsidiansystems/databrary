{-# LANGUAGE OverloadedStrings #-}
module Databrary.Store.Service
  ( Storage
  , initStorage
  ) where

import Control.Monad (unless, foldM_)
import Data.Maybe (catMaybes)
import System.Directory (getTemporaryDirectory, createDirectoryIfMissing)
import System.IO.Error (mkIOError, doesNotExistErrorType, illegalOperationErrorType)
import System.Posix.FilePath (addTrailingPathSeparator)
import System.Posix.Files.ByteString (isDirectory, deviceID)

import Databrary.Ops
import qualified Databrary.Store.Config as Conf
import Databrary.Files
import Databrary.Store.Types
import Databrary.Store.Transcoder

initStorage :: Conf.Config -> IO Storage
initStorage conf
  | Just down <- conf Conf.! "DOWN" = return $ error $ "Storage unavailable: " ++ down
  | otherwise = do
  temp <- fromMaybeM (toRawFilePath <$> getTemporaryDirectory) $ conf Conf.! "temp"

  foldM_ (\dev f -> do
    s <- getFileStatus f
    unless (isDirectory s)
      $ ioError $ mkIOError doesNotExistErrorType "storage directory" Nothing (Just (toFilePath f))
    let d = deviceID s
    unless (all (d ==) dev)
      $ ioError $ mkIOError illegalOperationErrorType "storage filesystem" Nothing (Just (toFilePath f))
    return $ Just d)
    Nothing $ catMaybes [Just master, Just temp, Just upload, stage]

  mapM_ (\c -> createDirectoryIfMissing False (toFilePath c </> "tmp")) cache

  tc <- initTranscoder (conf Conf.! "transcode")

  return $ Storage
    { storageMaster = master
    , storageFallback = conf Conf.! "fallback"
    , storageTemp = addTrailingPathSeparator temp
    , storageUpload = upload
    , storageCache = cache
    , storageStage = stage
    , storageTranscoder = tc
    }
  where
  master = conf Conf.! "master"
  upload = conf Conf.! "upload"
  cache = conf Conf.! "cache"
  stage = conf Conf.! "stage"
