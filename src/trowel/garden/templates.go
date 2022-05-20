package garden

import "text/template"

var (
	frontMatterTmpl   *template.Template
	backlinksTemplate *template.Template
)

func init() {
	frontMatterTmpl = template.Must(template.New("front-matter").Parse(`#+title: {{ .Title }}
#+description: {{ .Description }}
#+slug: {{ .Slug }}
#+tags: {{ .Tags }}
#+type: garden
#+lastmod: {{ .LastMod }}
`))
	backlinksTemplate = template.Must(template.New("backlinks").Parse(`
{{ if gt (len .Backlinks) 0 }}
* Backlinks
{{ range .Backlinks }}
- [[/garden/{{ .Slug }}][{{ .Title }}]]
{{ end }}
{{ end }}
`))
}
