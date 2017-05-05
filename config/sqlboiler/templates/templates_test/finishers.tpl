{{ if .Table.HasPrimaryKey }}
{{- $tableNameSingular := .Table.Name | singular | titleCase -}}
{{- $tableNamePlural := .Table.Name | plural | titleCase -}}
{{- $varNamePlural := .Table.Name | plural | camelCase -}}
{{- $varNameSingular := .Table.Name | singular | camelCase -}}
func test{{$tableNamePlural}}Bind(t *testing.T) {
	t.Parallel()

    {{template "isCustomSimple" .}}

	tx := MustTx(boil.Begin())
	defer tx.Rollback()
	if err = {{$varNameSingular}}.Insert(tx); err != nil {
		t.Error(err)
	}

	if err = {{$tableNamePlural}}(tx).Bind({{$varNameSingular}}); err != nil {
		t.Error(err)
	}
}

func test{{$tableNamePlural}}One(t *testing.T) {
	t.Parallel()

    {{template "isCustomSimple" .}}

	tx := MustTx(boil.Begin())
	defer tx.Rollback()
	if err = {{$varNameSingular}}.Insert(tx); err != nil {
		t.Error(err)
	}

	if x, err := {{$tableNamePlural}}(tx).One(); err != nil {
		t.Error(err)
	} else if x == nil {
		t.Error("expected to get a non nil record")
	}
}

func test{{$tableNamePlural}}All(t *testing.T) {
	t.Parallel()

    {{template "isCustomTwo" .}}

	tx := MustTx(boil.Begin())
	defer tx.Rollback()
	if err = {{$varNameSingular}}One.Insert(tx); err != nil {
		t.Error(err)
	}
	if err = {{$varNameSingular}}Two.Insert(tx); err != nil {
		t.Error(err)
	}

	slice, err := {{$tableNamePlural}}(tx).All()
	if err != nil {
		t.Error(err)
	}

	if len(slice) != 2 {
		t.Error("want 2 records, got:", len(slice))
	}
}

func test{{$tableNamePlural}}Count(t *testing.T) {
	t.Parallel()

    {{template "isCustomTwo" .}}

	tx := MustTx(boil.Begin())
	defer tx.Rollback()
	if err = {{$varNameSingular}}One.Insert(tx); err != nil {
		t.Error(err)
	}
	if err = {{$varNameSingular}}Two.Insert(tx); err != nil {
		t.Error(err)
	}

	count, err := {{$tableNamePlural}}(tx).Count()
	if err != nil {
		t.Error(err)
	}

	if count != 2 {
		t.Error("want 2 records, got:", count)
	}
}
{{end}}
