module Dom where

import Control.Monad.Eff (Eff(..))
import DOM.Node

newtype Element = Element Node

type Rect = 
  { bottom :: Number
  , height :: Number
  , left   :: Number
  , right  :: Number
  , top    :: Number
  , width  :: Number
  }

foreign import getBoundingClientRect """
  function (e) {
    return function () {
      return e.getBoundingClientRect();
    };
  }""":: forall e . Element -> Eff (dom :: DOM | e) Rect
