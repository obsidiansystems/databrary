module Util where

import Control.Monad.Eff (Eff())
import Data.StrMap (StrMap())

foreign import data UnsafeEff :: * -> *
foreign import unsafeEff """
  function unsafeEff(eff) {
    return eff();
  }""" :: forall e a . Eff e a -> UnsafeEff a

foreign import mapRange """
  function mapRange(f) {
    return function (l) {
      var r = [];
      for (var i = 0; i < l; i++)
        r.push(f(i));
      return r;
    };
  }""" :: forall a . (Number -> a) -> Number -> [a]

foreign import unsafeLookup """
  function unsafeLookup(m) {
    return function(k) {
      return m[k];
    };
  }""" :: forall a . StrMap a -> String -> a

foreign import invertArray """
  function invertArray(a) {
    var r = {};
    for (var i = 0; i < a.length; i ++)
      r[a[i]] = i;
    return r;
  }""" :: [String] -> StrMap Number

