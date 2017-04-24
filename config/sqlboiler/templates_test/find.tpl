{{- $tableNameSingular := .Table.Name | singular | titleCase -}}
{{- $tableNamePlural := .Table.Name | plural | titleCase -}}
{{- $varNamePlural := .Table.Name | plural | camelCase -}}
{{- $varNameSingular := .Table.Name | singular | camelCase -}}
{{- $hasCustom := .Table.HasCustom -}}
func test{{$tableNamePlural}}Find(t *testing.T) {
	t.Parallel()

	var err error
	seed := randomize.NewSeed()
    // this is a hack because if randomize isn't used compiler will complain
    // but if seed isn't then compiler will complain too
    _ = seed
	{{if not $hasCustom}}
	{{$varNameSingular}} := &{{$tableNameSingular}}{}
	if err = randomize.Struct(seed, {{$varNameSingular}}, {{$varNameSingular}}DBTypes, true, {{$varNameSingular}}ColumnsWithDefault...); err != nil {
		t.Errorf("Unable to randomize {{$tableNameSingular}} struct: %s", err)
	}
    {{else}}
    {{$varNameSingular}} := {{$tableNameSingular}}Random()
    {{end}}
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
