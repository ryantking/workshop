package garden

import (
	"fmt"
	"io"
	"os"
	"path/filepath"
	"strings"
	"time"

	"github.com/google/uuid"
	"github.com/niklasfasching/go-org/org"
	"github.com/ryantking/workshop/lib/cli/log"
	"github.com/ryantking/workshop/lib/funcy"
	"github.com/ryantking/workshop/src/trowel/roam"
	"github.com/spf13/afero"
)

// Gardener plants nodes in the garden.
type Gardener struct {
	idToSlug  map[string]string
	backlinks map[uuid.UUID][]roam.Stub
}

// NewGardener returns a new gardener and builds backlings from the given roam directory.
func NewGardener(ctx Context, roamDir roam.Directory) (*Gardener, error) {
	gardener := Gardener{
		idToSlug:  make(map[string]string, len(roamDir.Stubs)),
		backlinks: make(map[uuid.UUID][]roam.Stub),
	}
	gardener.buildSlugCache(roamDir)
	if err := gardener.buildBacklinks(ctx, roamDir); err != nil {
		return nil, err
	}

	return &gardener, nil
}

func (g *Gardener) buildSlugCache(roamDir roam.Directory) {
	for _, stub := range roamDir.Stubs {
		g.idToSlug["id:"+stub.ID.String()] = stub.Slug
	}
}

func (g *Gardener) buildBacklinks(ctx Context, roamDir roam.Directory) error {
	for _, stub := range roamDir.Stubs {
		doc, err := roam.OpenStub(ctx, stub)
		if err != nil {
			return err
		}

		for _, link := range g.findLinks(doc.Nodes) {
			if link.Protocol != "id" {
				continue
			}

			id, err := uuid.Parse(strings.TrimPrefix(link.URL, "id:"))
			if err != nil {
				return err
			}
			targetStub, ok := roamDir.Stub(id)
			if !ok {
				return fmt.Errorf("broken link: %s: %s", stub.Title, link.URL)
			}

			g.backlinks[stub.ID] = append(g.backlinks[stub.ID], *targetStub)
		}
	}

	for id, stubs := range g.backlinks {
		seenStubs := make(map[uuid.UUID]struct{}, len(stubs))
		g.backlinks[id] = funcy.Filter(stubs, func(stub roam.Stub) bool {
			_, ok := seenStubs[stub.ID]
			seenStubs[stub.ID] = struct{}{}
			return ok
		})
	}

	return nil
}

func (g Gardener) findLinks(nodes []org.Node) []org.RegularLink {
	links := make([]org.RegularLink, 0)
	for _, inode := range nodes {
		switch node := inode.(type) {
		case org.Headline:
			links = append(links, g.findLinks(node.Children)...)
		case org.Paragraph:
			links = append(links, g.findLinks(node.Children)...)
		case org.RegularLink:
			links = append(links, node)
		}
	}

	return links
}

// Plants puts a note in a garden.
func (g Gardener) Plant(ctx Context, stub roam.Stub) error {
	path := filepath.Join(ctx.GardenDir(), "content", "garden", filepath.Base(stub.Path)[15:])
	if yes, err := g.isUpToDate(ctx, stub, path); err != nil {
		return err
	} else if yes {
		log.Infofln(ctx, "%s up-to-date", stub.Title)
		return nil
	}
	log.Infofln(ctx, "converting %s: %s", stub.Title, path)
	f, err := g.create(ctx, path)
	if err != nil {
		return err
	}
	defer f.Close()
	doc, err := roam.OpenStub(ctx, stub)
	if err != nil {
		return err
	}
	doc.Nodes = g.filterPrivateNodes(doc.Nodes)
	doc.Nodes = g.convertLinks(ctx, doc.Nodes)
	return g.writeDocument(ctx, f, *doc)
}

func (g Gardener) isUpToDate(ctx Context, stub roam.Stub, path string) (bool, error) {
	info, err := ctx.FS().Stat(path)
	if os.IsNotExist(err) {
		return false, nil
	} else if err != nil {
		return false, err
	}

	return info.ModTime().After(stub.LastUpdated), nil
}

func (g Gardener) create(ctx Context, path string) (afero.File, error) {
	if err := ctx.FS().MkdirAll(filepath.Dir(path), 0o750); err != nil {
		return nil, fmt.Errorf("unable to create content directory: %w", err)
	}

	return ctx.FS().Create(path)
}

func (g Gardener) filterPrivateNodes(nodes []org.Node) []org.Node {
	return funcy.Reduce(nodes, func(nodes []org.Node, node org.Node) []org.Node {
		if hl, ok := node.(org.Headline); ok && funcy.Contains(hl.Tags, "private") {
			return nodes
		} else if ok {
			hl.Children = g.filterPrivateNodes(hl.Children)
			return append(nodes, hl)
		}

		return append(nodes, node)
	}, make([]org.Node, 0, len(nodes)))
}

func (g Gardener) convertLinks(ctx Context, nodes []org.Node) []org.Node {
	out := make([]org.Node, 0, len(nodes))
	for _, inode := range nodes {
		switch node := inode.(type) {
		case org.Headline:
			node.Children = g.convertLinks(ctx, node.Children)
			out = append(out, node)
		case org.Paragraph:
			node.Children = g.convertLinks(ctx, node.Children)
			out = append(out, node)
		case org.RegularLink:
			if node.Protocol == "id" {
				node.Protocol = ""
				node.URL = fmt.Sprintf("/garden/%s", g.idToSlug[node.URL])
			}
			out = append(out, node)
		default:
			out = append(out, node)
		}
	}

	return out
}

func (g Gardener) writeDocument(ctx Context, w io.Writer, doc roam.Document) error {
	if err := frontMatterTmpl.Execute(w, struct {
		Title, Description, Slug, Tags string
		LastMod                        time.Time
	}{
		Title:       doc.Title,
		Description: doc.Description,
		Slug:        doc.Slug,
		Tags:        strings.Join(doc.Tags, " "),
		LastMod:     doc.LastUpdated,
	}); err != nil {
		return err
	}

	for _, node := range doc.Nodes {
		fmt.Fprint(w, node.String())
	}

	return backlinksTemplate.Execute(w, struct {
		Backlinks []roam.Stub
	}{
		Backlinks: g.backlinks[doc.ID],
	})
}
