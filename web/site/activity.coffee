'use strict'

app.directive 'activity', [
  () ->
    restrict: 'E'
    templateUrl: 'site/activity.html'
    link: ($scope) ->
      $scope.idColor = (i) ->
        hsv = 3733*i
        sv = hsv/360|0
        "hsl(#{hsv%360},#{70+sv%20}%,#{53+(sv/20|0)%27}%)"
]
