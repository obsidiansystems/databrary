{{ if .Table.HasPrimaryKey }}
{{- $tableNameSingular := .Table.Name | singular | titleCase -}}
{{- $tableNamePlural := .Table.Name | plural | titleCase -}}
{{- $varNamePlural := .Table.Name | plural | camelCase -}}
{{- $varNameSingular := .Table.Name | singular | camelCase -}}
func test{{$tableNamePlural}}Exists(t *testing.T) {
	t.Parallel()

    {{template "isCustomSimple" .}}

	tx := MustTx(boil.Begin())
	defer tx.Rollback()
	if err = {{$varNameSingular}}.Insert(tx); err != nil {
		t.Error(err)
	}

	{{$pkeyArgs := .Table.PKey.Columns | stringMap .StringFuncs.titleCase | prefixStringSlice (printf "%s." $varNameSingular) | join ", " -}}
	e, err := {{$tableNameSingular}}Exists(tx, {{$pkeyArgs}})
	if err != nil {
		t.Errorf("Unable to check if {{$tableNameSingular}} exists: %s", err)
	}
	if !e {
		t.Errorf("Expected {{$tableNameSingular}}ExistsG to return true, but got false.")
	}
}
{{end}}
