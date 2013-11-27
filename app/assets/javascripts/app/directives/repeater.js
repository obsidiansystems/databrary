define(['app/modules/dbDirectives'], function (db) {
	'use strict';

	db.directive('repeater', [function () {
		var link = function ($scope, $element, $attrs) {
			$scope.repeats = $scope.repeats || [
				{}
			];

			$scope.getIndex = function (repeat) {
				return $scope.repeats.indexOf(repeat);
			};

			$scope.getRepeat = function (repeat) {
				return $scope.repeats[$scope.getIndex(repeat)];
			};

			$scope.createRepeat = function () {
				$scope.repeats.push({});

				return $scope.repeats.slice(-1)[0];
			};

			$scope.updateRepeat = function (old, repeat) {
				var index = $scope.getIndex(old);

				if (!~index)
					return false;

				$scope.repeats[index] = $.extend(true, {}, $scope.repeats[index], repeat);

				return $scope.repeats[index];
			};

			$scope.deleteRepeat = function (repeat) {
				var index = $scope.getIndex(repeat);

				if (!~index)
					return false;

				var deleted = $scope.repeats.splice(index, 1);

				if ($scope.repeats.length == 0)
					$scope.repeats.push({});

				return deleted;
			};

			$scope.isMoveable = function () {
				return false;
			}
		};

		return {
			restrict: 'A',
			scope: true,
			link: link
		}
	}]);
});
