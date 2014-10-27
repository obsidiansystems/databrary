module ClassificationSelect (Locals(), directive) where

import Control.Monad.Eff (returnE)
import Data.Array (findIndex)
import Angular.Scope (ReadWriteEff())
import Util (mapRange)
import Ng
import Constants

type Locals =
  ( value :: Number
  , name :: String
  , classification :: [String]
  , max :: Classification
  , check :: [Boolean]
  , update :: forall e . ReadWriteEff e Unit
  )

link scope element = updateScope init scope where 
  update :: Object Locals -> Object Locals
  update s = s { value = findIndex id s.check }
  init :: Object Locals -> Object Locals
  init s = s
    { classification = constants.classification
    , max = Constants.classification.public
    , check = mapRange ((<=) s.value) (Data.Array.length constants.classification)
    , update = updateScope update scope
    }

directive = returnE
  { restrict: "E"
  , templateUrl: "asset/classificationSelect.html"
  , scope: {
      value: "=ngModel"
    , name: "@"
    }
  , link: makeLink link
  }
