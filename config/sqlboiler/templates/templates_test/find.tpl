{{ if .Table.HasPrimaryKey }}
{{- $tableNameSingular := .Table.Name | singular | titleCase -}}
{{- $tableNamePlural := .Table.Name | plural | titleCase -}}
{{- $varNamePlural := .Table.Name | plural | camelCase -}}
{{- $varNameSingular := .Table.Name | singular | camelCase -}}
func test{{$tableNamePlural}}Find(t *testing.T) {
	t.Parallel()

    {{template "isCustomSimple" .}}

	tx := MustTx(boil.Begin())
	defer tx.Rollback()
	if err = {{$varNameSingular}}.Insert(tx); err != nil {
		t.Error(err)
	}

	{{$varNameSingular}}Found, err := Find{{$tableNameSingular}}(tx, {{.Table.PKey.Columns | stringMap .StringFuncs.titleCase | prefixStringSlice (printf "%s." $varNameSingular) | join ", "}})
	if err != nil {
		t.Error(err)
	}

	if {{$varNameSingular}}Found == nil {
		t.Error("want a record, got nil")
	}
}
{{end}}
