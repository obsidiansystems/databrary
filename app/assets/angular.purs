module Ng where

import Control.Monad.Eff (Eff(..))
import qualified Angular.Scope as Scope
import qualified Angular.Element as Element

modifyScope :: forall a e . (Object a -> Eff e (Object a)) -> Scope.Scope a -> Scope.ReadWriteEff e Unit
modifyScope = Scope.modifyScope

updateScope :: forall a e . (Object a -> Object a) -> Scope.Scope a -> Scope.ReadWriteEff e Unit
updateScope f = modifyScope (return <<< f)

type Link l e = Scope.Scope l -> Element.Element -> Eff e Unit

foreign import data LinkFn :: *

foreign import makeLink """
  function makeLink(link) {
    return function ($scope, $element) {
      return link($scope)($element)();
    };
  }""" :: forall l e . Link l e -> LinkFn
