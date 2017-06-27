{{- $tableNameSingular := .Table.Name | singular | titleCase -}}
{{- $tableNamePlural := .Table.Name | plural | titleCase -}}
{{- $varNamePlural := .Table.Name | plural | camelCase -}}
{{- $varNameSingular := .Table.Name | singular | camelCase -}}
func test{{$tableNamePlural}}(t *testing.T) {
	t.Parallel()

	query := {{$tableNamePlural}}(nil)

	if query.Query == nil {
		t.Error("expected a query, got nothing")
	}
}
{{ if .Table.HasPrimaryKey }}
func test{{$tableNamePlural}}Live(t *testing.T) {
		all, err := {{$tableNamePlural}}(dbMain.liveDbConn).All()
		if err != nil {
			t.Fatalf("failed to get all {{$tableNamePlural}} err: ", err)
		}
		tx, err := dbMain.liveTestDbConn.Begin()
		if err != nil {
			t.Fatalf("failed to begin transaction: ", err)
		}
		for _, v := range all {
			err := v.Insert(tx)
			if err != nil {
			t.Fatalf("failed to failed to insert %s because of %s", v, err)
			}

		}
		err = tx.Commit()
		if err != nil {
			t.Fatalf("failed to commit transaction: ", err)
		}
		bf := &bytes.Buffer{}
		dumpCmd := exec.Command("psql", `-c "COPY (SELECT * FROM {{.Table.Name}}) TO STDOUT" -d `, dbMain.DbName)
		dumpCmd.Env = append(os.Environ(), dbMain.pgEnv()...)
		dumpCmd.Stdout = bf
		err = dumpCmd.Start()
        if err != nil {
            t.Fatalf("failed to start dump from live db because of %s", err)
        }
		dumpCmd.Wait()
        if err != nil {
            t.Fatalf("failed to wait dump from live db because of %s", err)
        }
		bg := &bytes.Buffer{}
        dumpCmd = exec.Command("psql", `-c "COPY (SELECT * FROM {{.Table.Name}}) TO STDOUT" -d `, dbMain.LiveTestDBName)
		dumpCmd.Env = append(os.Environ(), dbMain.pgEnv()...)
		dumpCmd.Stdout = bg
		err = dumpCmd.Start()
        if err != nil {
            t.Fatalf("failed to start dump from test db because of %s", err)
        }
		dumpCmd.Wait()
        if err != nil {
            t.Fatalf("failed to wait dump from test db because of %s", err)
        }
		bfslice := sort.StringSlice(difflib.SplitLines(bf.String()))
		gfslice := sort.StringSlice(difflib.SplitLines(bg.String()))
		bfslice.Sort()
		gfslice.Sort()
		diff := difflib.ContextDiff{
			A: bfslice,
			B: gfslice,
			FromFile: "databrary",
			ToFile: "test",
			Context: 1,
		}
		result, _ := difflib.GetContextDiffString(diff)
		if len(result)>0 {
		    t.Fatalf("{{$tableNamePlural}}Live failed but it's probably trivial: %s", strings.Replace(result, "\t", " ", -1))
		}

	}
{{end}}
