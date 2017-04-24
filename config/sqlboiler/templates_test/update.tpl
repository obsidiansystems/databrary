{{- $tableNameSingular := .Table.Name | singular | titleCase -}}
{{- $tableNamePlural := .Table.Name | plural | titleCase -}}
{{- $varNamePlural := .Table.Name | plural | camelCase -}}
{{- $varNameSingular := .Table.Name | singular | camelCase -}}
{{- $hasCustom := .Table.HasCustom -}}
func test{{$tableNamePlural}}Update(t *testing.T) {
	t.Parallel()

	if len({{$varNameSingular}}Columns) == len({{$varNameSingular}}PrimaryKeyColumns) {
		t.Skip("Skipping table with only primary key columns")
	}

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

    {{if not $hasCustom}}
	if err = randomize.Struct(seed, {{$varNameSingular}}, {{$varNameSingular}}DBTypes, true, {{$varNameSingular}}ColumnsWithDefault...); err != nil {
		t.Errorf("Unable to randomize {{$tableNameSingular}} struct: %s", err)
	}
	{{else}}
    {{$varNameSingular}} = {{$tableNameSingular}}Random()
    {{end}}

	if err = {{$varNameSingular}}.Update(tx); err != nil {
		t.Error(err)
	}
}

func test{{$tableNamePlural}}SliceUpdateAll(t *testing.T) {
	t.Parallel()

	if len({{$varNameSingular}}Columns) == len({{$varNameSingular}}PrimaryKeyColumns) {
		t.Skip("Skipping table with only primary key columns")
	}

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
    {{if not $hasCustom}}
	if err = randomize.Struct(seed, {{$varNameSingular}}, {{$varNameSingular}}DBTypes, true, {{$varNameSingular}}PrimaryKeyColumns...); err != nil {
		t.Errorf("Unable to randomize {{$tableNameSingular}} struct: %s", err)
	}
	{{else}}
    {{$varNameSingular}} = {{$tableNameSingular}}Random()
    {{end}}

	// Remove Primary keys and unique columns from what we plan to update
	var fields []string
	if strmangle.StringSliceMatch({{$varNameSingular}}Columns, {{$varNameSingular}}PrimaryKeyColumns) {
		fields = {{$varNameSingular}}Columns
	} else {
		fields = strmangle.SetComplement(
			{{$varNameSingular}}Columns,
			{{$varNameSingular}}PrimaryKeyColumns,
		)
	}

	value := reflect.Indirect(reflect.ValueOf({{$varNameSingular}}))
	updateMap := M{}
	for _, col := range fields {
		updateMap[col] = value.FieldByName(strmangle.TitleCase(col)).Interface()
	}

	slice := {{$tableNameSingular}}Slice{{"{"}}{{$varNameSingular}}{{"}"}}
	if err = slice.UpdateAll(tx, updateMap); err != nil {
		t.Error(err)
	}
}
