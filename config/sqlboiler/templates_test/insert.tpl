{{- $tableNameSingular := .Table.Name | singular | titleCase -}}
{{- $tableNamePlural := .Table.Name | plural | titleCase -}}
{{- $varNamePlural := .Table.Name | plural | camelCase -}}
{{- $varNameSingular := .Table.Name | singular | camelCase -}}
{{- $parent := . -}}
{{- $hasCustom := .Table.HasCustom -}}
func test{{$tableNamePlural}}Insert(t *testing.T) {
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

	count, err := {{$tableNamePlural}}(tx).Count()
	if err != nil {
		t.Error(err)
	}

	if count != 1 {
		t.Error("want one record, got:", count)
	}
}

func test{{$tableNamePlural}}InsertWhitelist(t *testing.T) {
	t.Parallel()

	var err error
	seed := randomize.NewSeed()
    // this is a hack because if randomize isn't used compiler will complain
    // but if seed isn't then compiler will complain too
    _ = seed
	{{if not $hasCustom}}
	{{$varNameSingular}} := &{{$tableNameSingular}}{}
	if err = randomize.Struct(seed, {{$varNameSingular}}, {{$varNameSingular}}DBTypes, true); err != nil {
		t.Errorf("Unable to randomize {{$tableNameSingular}} struct: %s", err)
	}
    {{else}}
    {{$varNameSingular}} := {{$tableNameSingular}}Random()
    {{end}}

	tx := MustTx(boil.Begin())
	defer tx.Rollback()
	if err = {{$varNameSingular}}.Insert(tx, {{$varNameSingular}}Columns...); err != nil {
		t.Error(err)
	}

	count, err := {{$tableNamePlural}}(tx).Count()
	if err != nil {
		t.Error(err)
	}

	if count != 1 {
		t.Error("want one record, got:", count)
	}
}
