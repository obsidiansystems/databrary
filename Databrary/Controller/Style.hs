{-# LANGUAGE OverloadedStrings #-}
module Databrary.Controller.Style
  ( viewStyle
  ) where

import Databrary.Has
import Databrary.Action.Types
import Databrary.Action
import Databrary.HTTP.Path.Parser
import Databrary.Controller.Angular

viewStyle :: ActionRoute ()
viewStyle = action GET (pathHTML >/> "admin" >/> "style") $ \() -> withoutAuth $ do
  angular
  peeks notFoundResponse
