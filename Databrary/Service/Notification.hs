{-# LANGUAGE OverloadedStrings #-}
module Databrary.Service.Notification
  ( Notifications(..)
  , initNotifications
  , triggerNotifications
  ) where

import Control.Concurrent.MVar (MVar, newMVar, tryPutMVar, tryTakeMVar)
import Control.Monad (unless)
import qualified Data.ByteString as BS
import qualified Text.Regex.Posix as Regex

import Databrary.Model.Periodic (Period)
import qualified Databrary.Store.Config as Conf

data Notifications = Notifications
  { notificationsTrigger :: !(MVar (Maybe Period))
  , notificationsFilter :: !Regex.Regex
  , notificationsCopy :: !(Maybe BS.ByteString)
  }

initNotifications :: Conf.Config -> IO Notifications
initNotifications conf = do
  t <- newMVar Nothing -- run async notification pass at boot
  return Notifications
    { notificationsTrigger = t
    , notificationsFilter = Regex.makeRegexOpts Regex.compIgnoreCase Regex.blankExecOpt (conf Conf.! "filter" :: BS.ByteString)
    , notificationsCopy = conf Conf.! "copy"
    }

triggerNotifications :: Maybe Period -> Notifications -> IO ()
triggerNotifications p Notifications{ notificationsTrigger = t } = write p where
  -- poor man's LVar: would be much simpler with a nested MVar, but then the reader is harder (and ours is dumb anyway)
  write x = do
    r <- tryPutMVar t x
    unless r $ do
      y <- tryTakeMVar t
      write $ maybe id max y x
