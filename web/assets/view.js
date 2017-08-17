// Generated by CoffeeScript 1.12.6
'use strict';
app.controller('asset/view', [
  '$scope', 'displayService', 'asset', function($scope, display, asset) {
    $scope.close = function() {
      return window.history.back();
    };
    $scope.asset = asset;
    display.title = asset.displayName;
    $scope.volume = asset.volume;
    return $scope.hasThumbnail = asset.format.type === 'image' || asset.format.type === 'video' && asset.duration && !asset.pending;
  }
]);

//# sourceMappingURL=view.js.map
