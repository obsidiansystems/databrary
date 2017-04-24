{{- $tableNameSingular := .Table.Name | singular | titleCase -}}
{{- $tableNamePlural := .Table.Name | plural | titleCase -}}
{{- $varNamePlural := .Table.Name | plural | camelCase -}}
{{- $varNameSingular := .Table.Name | singular | camelCase -}}
{{- $hasCustom := .Table.HasCustom -}}
func test{{$tableNamePlural}}Upsert(t *testing.T) {
	t.Parallel()

	if len({{$varNameSingular}}Columns) == len({{$varNameSingular}}PrimaryKeyColumns) {
		t.Skip("Skipping table with only primary key columns")
	}

	var err error
	seed := randomize.NewSeed()
    // this is a hack because if randomize isn't used compiler will complain
    // but if seed isn't then compiler will complain too
    _ = seed
	// Attempt the INSERT side of an UPSERT
	{{if not $hasCustom}}
	{{$varNameSingular}} := {{$tableNameSingular}}{}
	if err = randomize.Struct(seed, &{{$varNameSingular}}, {{$varNameSingular}}DBTypes, true); err != nil {
		t.Errorf("Unable to randomize {{$tableNameSingular}} struct: %s", err)
	}
	{{else}}
    {{$varNameSingular}} := {{$tableNameSingular}}Random()
    {{end}}

	tx := MustTx(boil.Begin())
	defer tx.Rollback()
	if err = {{$varNameSingular}}.Upsert(tx, {{if eq .DriverName "postgres"}}false, nil, {{end}}nil); err != nil {
		t.Errorf("Unable to upsert {{$tableNameSingular}}: %s", err)
	}

	count, err := {{$tableNamePlural}}(tx).Count()
	if err != nil {
		t.Error(err)
	}
	if count != 1 {
		t.Error("want one record, got:", count)
	}

	// Attempt the UPDATE side of an UPSERT
	{{if not $hasCustom}}
	if err = randomize.Struct(seed, &{{$varNameSingular}}, {{$varNameSingular}}DBTypes, false, {{$varNameSingular}}PrimaryKeyColumns...); err != nil {
		t.Errorf("Unable to randomize {{$tableNameSingular}} struct: %s", err)
	}
	{{else}}
    {{$varNameSingular}} = {{$tableNameSingular}}Random()
    {{end}}

	if err = {{$varNameSingular}}.Upsert(tx, {{if eq .DriverName "postgres"}}true, nil, {{end}}nil); err != nil {
		t.Errorf("Unable to upsert {{$tableNameSingular}}: %s", err)
	}

	count, err = {{$tableNamePlural}}(tx).Count()
	if err != nil {
		t.Error(err)
	}
	if count != 1 {
		t.Error("want one record, got:", count)
	}
}
