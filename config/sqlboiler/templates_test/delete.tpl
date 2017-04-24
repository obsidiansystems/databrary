{{- $tableNameSingular := .Table.Name | singular | titleCase -}}
{{- $tableNamePlural := .Table.Name | plural | titleCase -}}
{{- $varNamePlural := .Table.Name | plural | camelCase -}}
{{- $varNameSingular := .Table.Name | singular | camelCase -}}
{{- $hasCustom := .Table.HasCustom -}}
func test{{$tableNamePlural}}Delete(t *testing.T) {
	t.Parallel()

    var err error
    seed := randomize.NewSeed()
    // this is a hack because if randomize isn't used compiler will complain
    // but if seed isn't then compiler will complain too
    _ = seed
    {{if not $hasCustom }}
    {{$varNameSingular}} := &{{$tableNameSingular}}{}
    if err = randomize.Struct(seed, {{$varNameSingular}}, {{$varNameSingular}}DBTypes, true); err != nil {
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

	if err = {{$varNameSingular}}.Delete(tx); err != nil {
		t.Error(err)
	}

	count, err := {{$tableNamePlural}}(tx).Count()
	if err != nil {
		t.Error(err)
	}

	if count != 0 {
		t.Error("want zero records, got:", count)
	}
}

func test{{$tableNamePlural}}QueryDeleteAll(t *testing.T) {
	t.Parallel()

	var err error
	seed := randomize.NewSeed()
    // this is a hack because if randomize isn't used compiler will complain
    // but if seed isn't then compiler will complain too
    _ = seed
	{{if not $hasCustom }}
	{{$varNameSingular}} := &{{$tableNameSingular}}{}
	if err = randomize.Struct(seed, {{$varNameSingular}}, {{$varNameSingular}}DBTypes, true); err != nil {
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

	if err = {{$tableNamePlural}}(tx).DeleteAll(); err != nil {
		t.Error(err)
	}

	count, err := {{$tableNamePlural}}(tx).Count()
	if err != nil {
		t.Error(err)
	}

	if count != 0 {
		t.Error("want zero records, got:", count)
	}
}

func test{{$tableNamePlural}}SliceDeleteAll(t *testing.T) {
	t.Parallel()

	var err error
	seed := randomize.NewSeed()
    // this is a hack because if randomize isn't used compiler will complain
    // but if seed isn't then compiler will complain too
    _ = seed
	{{if not $hasCustom }}
	{{$varNameSingular}} := &{{$tableNameSingular}}{}
	if err = randomize.Struct(seed, {{$varNameSingular}}, {{$varNameSingular}}DBTypes, true); err != nil {
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

	slice := {{$tableNameSingular}}Slice{{"{"}}{{$varNameSingular}}{{"}"}}

	if err = slice.DeleteAll(tx); err != nil {
		t.Error(err)
	}

	count, err := {{$tableNamePlural}}(tx).Count()
	if err != nil {
		t.Error(err)
	}

	if count != 0 {
		t.Error("want zero records, got:", count)
	}
}
