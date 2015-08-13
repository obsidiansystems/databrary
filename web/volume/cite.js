'use strict';

app.directive('citeVolume', [
  'constantService', 'routerService', '$location','page',
  function (constants, router, $location, page) {
    var link = function ($scope) {
      var volume = $scope.volume;

      var authors = _.map(volume.owners, function (owner) {
        var author;

        var i = owner.lastIndexOf(', '); // could equally incorrectly be indexOf
        if (i < 0)
          author = owner;
        else {
          i += 2;
          author = owner.substr(0, i);
          do {
            while (owner.charAt(i) == ' ')
              i++;
            author += owner.charAt(i) + '.';
          } while ((i = owner.indexOf(' ', i)+1) > 0);
        }
        return author;
      });

      var runRIS = function(){
        var route = {
          route: '/citation/ris',
          method: 'GET'
        };

        var routePromise = router.http(route, {id: $scope.volume.id});

        routePromise.then(function(data){
          $scope.citeText = data;
        }, function(err){
          console.log("Error:", err);
        });
      }; 
      
      var runBibTeX = function(){
        var route = {
          route: '/citation/bibtex',
          method: 'GET'
        };

        var routePromise = router.http(route, $scope.volume);

        routePromise.then(function(data){
          $scope.citeText = data; 
        }, function(error){
          console.log("Error: ", error);
        }); 
        
        // var bibTeXString = "@data{\n";
        // bibTeXString += " author  = " + $scope.authors + ",\n";
        // bibTeXString += " title   = " + $scope.volume + ", \n";
        // bibTeXString += "}";
        // return bibTeXString; 
      };

      $scope.getCitation = function(citationType){
        
        if(citationType == 'bibtex'){
          return runBibTeX(); 
        } else if (citationType == 'RIS'){
          return runRIS(); 
        }
      }; 
      authors.push(authors.splice(-2, 2).join(' & '));
      $scope.authors = authors.join(', ');
      $scope.today = new Date();
      $scope.permalink = (volume.doi ? 'doi:' + volume.doi : $location.absUrl());
    };

    return {
      restrict: 'E',
      templateUrl: 'volume/cite.html',
      scope: false,
      replace: true,
      link: link
    };
  }
]);
