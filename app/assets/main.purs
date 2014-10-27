module Main (main) where

import Angular.Module
import Angular.DI (annotate)

import Constants (Constants(..))

foreign import app :: Module

main = do
  directive "classificationSelect" (annotate ClassificationSelect.directive) app
  directive "tooltip" (annotate Tooltip.directive) app
