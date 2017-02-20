module Main (main) where

import Databrary.Service.DB (runDBConnection)
import Databrary.Service.DB.Schema

main :: IO ()
main = do
  let unattendedUpdate = True
  runDBConnection $ updateDBSchema "schema" unattendedUpdate
