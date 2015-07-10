'use strict';

app.directive('citeVolume', [
  'constantService', 'routerService', '$location',
  function (constants, router, $location) {
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

      var runBibTeX = function(){
        var bibTeXString = "{\n";
        bibTeXString += " author  = " + $scope.authors + ",\n";
        bibTeXString += " title   = " + $scope.volume + ", \n";
        bibTeXString += "}";
        return bibTeXString; 
      };

      var runRIS = function(){};
      
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
