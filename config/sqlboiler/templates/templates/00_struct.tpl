{{- define "relationship_to_one_struct_helper" -}}
{{- end -}}

{{- $dot := . -}}
{{- $tableNameSingular := .Table.Name | singular -}}
{{- $modelName := $tableNameSingular | titleCase -}}
{{- $modelNameCamel := $tableNameSingular | camelCase -}}
// {{$modelName}} is an object representing the database {{if not .Table.IsView }}table{{else}}view{{end}}.
type {{$modelName}} struct {
	{{range $column := .Table.Columns -}}
	{{titleCase $column.Name}} {{$column.Type}} `{{generateTags $dot.Tags $column.Name}}db:"{{$column.Name}}" json:"{{$modelNameCamel}}_{{ $column.Name}}{{if $column.Nullable}},omitempty{{end}}"`
	{{end -}}
	{{- if .Table.IsJoinTable -}}
	{{- else}}
	R *{{$modelNameCamel}}R `{{generateIgnoreTags $dot.Tags}}db:"-" json:"-"`
	L {{$modelNameCamel}}L `{{generateIgnoreTags $dot.Tags}}db:"-" json:"-"`
	{{end -}}
}

{{- if .Table.IsJoinTable -}}
{{- else}}
// {{$modelNameCamel}}R is where relationships are stored.
type {{$modelNameCamel}}R struct {
	{{range .Table.FKeys -}}
	{{- $txt := txtsFromFKey $dot.Tables $dot.Table . -}}
	{{$txt.Function.Name}} *{{$txt.ForeignTable.NameGo}}
	{{end -}}

	{{range .Table.ToOneRelationships -}}
	{{- $txt := txtsFromOneToOne $dot.Tables $dot.Table . -}}
	{{$txt.Function.Name}} *{{$txt.ForeignTable.NameGo}}
	{{end -}}

	{{range .Table.ToManyRelationships -}}
	{{- $txt := txtsFromToMany $dot.Tables $dot.Table . -}}
	{{$txt.Function.Name}} {{$txt.ForeignTable.Slice}}
	{{end -}}{{/* range tomany */}}
}

// {{$modelNameCamel}}L is where Load methods for each relationship are stored.
type {{$modelNameCamel}}L struct{}
{{end -}}
