package roam

import (
	"fmt"
	"strings"
	"time"

	"github.com/google/uuid"
	"github.com/niklasfasching/go-org/org"
	"github.com/ryantking/workshop/lib/funcy"
)

// Stub holds meta information about a document and is able to open the full document.
type Stub struct {
	// ID is the UUID of the org document from the top level propery drawer.
	ID uuid.UUID

	// Title is the title of the roam note.
	Title string

	// Slug is the key used in URLs that point to the document.
	Slug string

	// Tags are the filetags found.
	Tags []string

	// Description is the description of the document. This is assumed to be all
	// paragraph blocks that occur before the first headline.
	Description string

	// LastUpdated holds when the document was last updated.
	LastUpdated time.Time

	// Path is the location on disk that holds the full document
	Path string
}

// ParseStub parses the metadata out of an org document and returns a stub.
func ParseStub(ctx Context, doc org.Document) (*Stub, error) {
	info, err := ctx.FS().Stat(doc.Path)
	if err != nil {
		return nil, err
	}

	stub := Stub{
		LastUpdated: info.ModTime(),
		Path:        doc.Path,
	}

	for _, inode := range doc.Nodes {
		switch node := inode.(type) {
		case org.PropertyDrawer:
			id, ok := node.Get("ID")
			if !ok {
				return nil, fmt.Errorf("document does not have ID: %s", doc.Path)
			}
			stub.ID, err = uuid.Parse(id)
			if err != nil {
				return nil, fmt.Errorf("could not parse ID: %s: %w", doc.Path, err)
			}
		case org.Keyword:
			switch node.Key {
			case "TITLE":
				stub.Title = node.Value
				stub.Slug = strings.Replace(strings.ToLower(node.Value), "_", "-", -1)
			case "FILETAGS":
				stub.Tags = funcy.Filter(strings.Split(node.Value, ":"), func(tag string) bool {
					return tag == ""
				})
			}
		case org.Paragraph:
			stub.Description += strings.TrimSpace(node.String())
		case org.Headline:
			break
		}
	}

	if stub.ID == uuid.Nil {
		return nil, fmt.Errorf("%s: document has no ID", doc.Path)
	}
	if stub.Title == "" {
		return nil, fmt.Errorf("%s: document has no title", doc.Path)
	}

	return &stub, nil
}

// OpenStub opens the full document that a stub points to.
func OpenStub(ctx Context, stub Stub) (*Document, error) {
	doc, err := readOrgFile(ctx, stub.Path)
	if err != nil {
		return nil, err
	}

	return buildDocument(stub, *doc)
}
