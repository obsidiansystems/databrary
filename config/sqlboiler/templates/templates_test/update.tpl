{{ if .Table.HasPrimaryKey }}
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

    {{template "isCustomSimple" .}}

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

    blacklist := {{$varNameSingular}}ColumnsWithDefault
    {{- if $hasCustom}}
    blacklist = append(blacklist, {{$varNameSingular}}ColumnsWithCustom...)
    {{end}}

	if err = randomize.Struct(seed, {{$varNameSingular}}, {{$varNameSingular}}DBTypes, true, blacklist...); err != nil {
		t.Errorf("Unable to randomize {{$tableNameSingular}} struct: %s", err)
	}

    {{if $hasCustom}}
    {{template "customRandomRangeOne" .}}
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

    {{template "isCustomSimple" .}}

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

    blacklist := {{$varNameSingular}}PrimaryKeyColumns
    {{- if $hasCustom}}
    blacklist = append(blacklist, {{$varNameSingular}}ColumnsWithCustom...)
    {{end}}

	if err = randomize.Struct(seed, {{$varNameSingular}}, {{$varNameSingular}}DBTypes, true, blacklist...); err != nil {
		t.Errorf("Unable to randomize {{$tableNameSingular}} struct: %s", err)
	}

    {{if $hasCustom}}
    {{template "customRandomRangeOne" .}}
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
{{end}}
