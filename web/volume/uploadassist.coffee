 'use strict'

 app.directive 'volumeUploadAssist', [
  'constantService','messageService'
  (constants, messages) ->
    restrict: 'E'
    templateUrl: 'volume/uploadassist.html'
    link: ($scope) ->
      volume = $scope.volume

  ]