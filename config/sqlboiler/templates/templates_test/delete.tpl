{{ if .Table.HasPrimaryKey }}
{{- $tableNameSingular := .Table.Name | singular | titleCase -}}
{{- $tableNamePlural := .Table.Name | plural | titleCase -}}
{{- $varNamePlural := .Table.Name | plural | camelCase -}}
{{- $varNameSingular := .Table.Name | singular | camelCase -}}
func test{{$tableNamePlural}}Delete(t *testing.T) {
	t.Parallel()

    {{template "isCustomSimple" .}}

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

    {{template "isCustomSimple" .}}

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

    {{template "isCustomSimple" .}}

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
{{end}}
