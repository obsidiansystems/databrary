
{{define "customRandom"}}
    {{- $varNameSingular := .Table.Name | singular | camelCase -}}
    {{- $tableNameSingular := .Table.Name | singular | titleCase -}}
    if err = randomize.Struct(seed, {{$varNameSingular}}, {{$varNameSingular}}DBTypes, true, {{$varNameSingular}}ColumnsWithCustom...); err != nil {
        t.Errorf("Unable to randomize {{$tableNameSingular}} struct: %s", err)
    }
{{end}}

{{define "nonCustomRandom"}}
    {{- $varNameSingular := .Table.Name | singular | camelCase -}}
    {{- $tableNameSingular := .Table.Name | singular | titleCase -}}
    if err = randomize.Struct(seed, {{$varNameSingular}}, {{$varNameSingular}}DBTypes, true); err != nil {
        t.Errorf("Unable to randomize {{$tableNameSingular}} struct: %s", err)
    }
{{end}}


{{define "customRandomRangeOne"}}
    {{- $varNameSingular := .Table.Name | singular | camelCase -}}
    {{range $i, $v := .Table.GetCustomColumns -}}
    {{$varNameSingular}}.{{$v.Name | titleCase}} = {{$v.Type}}Random()
    {{end -}}
{{end}}

{{define "customRandomRangeTwo"}}
    {{- $varNameSingular := .Table.Name | singular | camelCase -}}
    {{range $i, $v := .Table.GetCustomColumns -}}
    {{$varNameSingular}}One.{{$v.Name | titleCase}} = {{$v.Type}}Random()
    {{$varNameSingular}}Two.{{$v.Name | titleCase}} = {{$v.Type}}Random()
    {{end -}}
{{end}}

{{define "isCustomSimple"}}
    {{- $hasCustom := .Table.HasCustom -}}
    {{- $varNameSingular := .Table.Name | singular | camelCase -}}
    {{- $tableNameSingular := .Table.Name | singular | titleCase -}}
    var err error
    seed := randomize.NewSeed()
    {{$varNameSingular}} := &{{$tableNameSingular}}{}
    {{- if not $hasCustom }}
    {{template "nonCustomRandom" . }}
    {{else}}
    {{template "customRandom" . }}
    {{template "customRandomRangeOne" . }}
    {{end}}
{{end}}

{{/*     {{template "isCustomSimple" .}}    */}}


{{define "isCustomTwo"}}
    {{- $hasCustom := .Table.HasCustom -}}
    {{- $varNameSingular := .Table.Name | singular | camelCase -}}
    {{- $tableNameSingular := .Table.Name | singular | titleCase -}}
	var err error
	seed := randomize.NewSeed()
	{{$varNameSingular}}One := &{{$tableNameSingular}}{}
	{{$varNameSingular}}Two := &{{$tableNameSingular}}{}
	if err = randomize.Struct(seed, {{$varNameSingular}}One, {{$varNameSingular}}DBTypes, false, {{$varNameSingular}}ColumnsWith{{if not $hasCustom}}Default{{else}}Custom{{end}}...); err != nil {

		t.Errorf("Unable to randomize {{$tableNameSingular}} struct: %s", err)
	}
	if err = randomize.Struct(seed, {{$varNameSingular}}Two, {{$varNameSingular}}DBTypes, false, {{$varNameSingular}}ColumnsWith{{if not $hasCustom}}Default{{else}}Custom{{end}}...); err != nil {
		t.Errorf("Unable to randomize {{$tableNameSingular}} struct: %s", err)
	}
    {{if $hasCustom}}
    {{template "customRandomRangeTwo" . }}
    {{end}}
{{end}}

{{/*     {{template "isCustomTwo" .}}    */}}



