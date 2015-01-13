'use strict'

app.directive 'inputCompleter', [
  'constantService',
  (constants) ->
    restrict: 'E'
    templateUrl: 'site/inputCompleter.html'
    scope:
      value: '=ngModel'
      completer: '&'
      placeholder: '@'
      pattern: '@ngPattern'
      submit: '&'
    link: ($scope, $element, $attrs) ->
      input = $element[0].firstChild
      # this doesn't happen early enough with normal angular binding:
      input.setAttribute('name', $attrs.inputName)
      min = 3 unless isFinite(min = parseInt($attrs.min))
      sent = resend = undefined
      $scope.choices = []

      setValue = (v) ->
        # we bypass the angular model here to change text and selection...
        old = input.value
        input.value = v
        if v.toLowerCase().startsWith(old.toLowerCase())
          input.setSelectionRange(old.length, v.length)

      handle = (r) ->
        if r && typeof r.then == 'function'
          r.then(handle)
        else
          sent = undefined
          if Array.isArray(r)
            if 'input' of r
              setValue(r.input)
              delete r.input
            if r.length
              $scope.choices = r
            else
              $scope.choices = [
                text: constants.message('search.none')
              ]
          else if r || r == ''
            setValue(r)
            $scope.choices = []
          $scope.search(resend) if resend

      $scope.search = () ->
        value = input.value
        if sent
          resend = value != sent && value
        else if value?.length >= min
          resend = undefined
          sent = value
          $scope.choices.push
            text: constants.message('search.active')
          handle($scope.completer({$input:value}))
        else
          $scope.choices = []

      $scope.choose = (c) ->
        resend = undefined
        handle(
          if typeof c.select == 'function'
            c.select()
          else
            c.select)

      $scope.enter = ($event) ->
        # bypass debounce
        $scope.value = value = input.value
        handle($scope.submit({$event:$event, $input:value})) if value?.length >= min

      $scope.search(input.value)

      return
]
