package roam

import (
	"github.com/niklasfasching/go-org/org"
)

// Document is an org roam document.
// The metadata nodes have been parsed into the struct.
type Document struct {
	Stub
	org.Document
}

// Open takes in a path and returns a paresd org roam document.
func Open(ctx Context, path string) (*Document, error) {
	doc, err := readOrgFile(ctx, path)
	if err != nil {
		return nil, err
	}
	stub, err := ParseStub(ctx, *doc)
	if err != nil {
		return nil, err
	}

	return buildDocument(*stub, *doc)
}

func readOrgFile(ctx Context, path string) (*org.Document, error) {
	f, err := ctx.FS().Open(path)
	if err != nil {
		return nil, err
	}
	defer f.Close()

	doc := org.New().Parse(f, path)
	if doc.Error != nil {
		return nil, doc.Error
	}

	return doc, nil
}

func buildDocument(stub Stub, doc org.Document) (*Document, error) {
	for len(doc.Nodes) > 0 {
		if _, ok := doc.Nodes[0].(org.Headline); ok {
			break
		}
		doc.Nodes = doc.Nodes[1:]
	}

	return &Document{
		Stub:     stub,
		Document: doc,
	}, nil
}
