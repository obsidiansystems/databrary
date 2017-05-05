{{- $notVoid := (ne .Proc.ReturnType "void") -}}
{{- $proc := (schema .Schema .Proc.ProcName) -}}
{{- if ne .Proc.ReturnType "trigger" -}}
// {{ .Name }} calls the stored procedure '{{ $proc }}({{ .ProcParams }}) {{ .Proc.ReturnType }}' on db.
func {{ .Name }}(exec boil.Executor{{ goparamlist .Params true true }}) ({{ if $notVoid }}{{ retype .Return.Type }}, {{ end }}error) {
	var err error

	// sql query
	const query = `SELECT {{ $proc }}({{ colvals .Params }})`

	// run query
{{- if $notVoid }}
	var ret {{ retype .Return.Type }}

	err = exec.QueryRow(query{{ goparamlist .Params true false }}).Scan(&ret)
	if err != nil {
		return {{ reniltype .Return.NilType }}, err
	}

	return ret, nil
{{- else }}
	if boil.DebugMode {
		fmt.Fprintf(boil.DebugWriter, "%s\n%v\n", query {{ goparamlist .Params true false }})
	}
	_, err = exec.Exec(query)
	return err
{{- end }}
}
{{- end }}


