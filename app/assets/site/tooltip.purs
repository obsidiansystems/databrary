module Tooltip (directive) where

import Control.Monad.Eff (returnE)
import Angular.Element (Element(..))
import qualified Angular.Timeout as Timeout
import qualified Dom as Dom
import Ng

type Tooltip =
  { cls :: String
  , target :: Dom.Element
  }

type Locals =
  ( tooltip :: Tooltip
  , classes :: [String]
  , style :: 
    { left :: Number
    , top :: Number
    }
  )

link timeoutS scope element = do
  updateScope init scope
  void $ Timeout.timeout'' (modifyScope position scope) timeoutS
  where
  init :: Object Locals -> Object Locals
  init s = s
    { classes = [s.tooltip.cls]
    , style = { left: 0, top: 0 }
    }
  position s = do
    let tooltip = s.tooltip
    return s

directive timeoutS = returnE
  { restrict: "E"
  , scope: false
  , require: "^tooltips"
  , templateUrl: "site/tooltip.html"
  , link: makeLink (link timeoutS) 
  }
