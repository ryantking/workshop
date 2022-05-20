package roam

import (
	"path/filepath"
	"strings"

	"github.com/google/uuid"
	"github.com/ryantking/workshop/lib/cli/log"
	"github.com/spf13/afero"
)

// Directory stores a bunch of roam documents.
// It only stores stubs so the documents can be loaded as needed.
type Directory struct {
	// Stubs contains a list of all files store in the directory.
	Stubs []Stub

	stubsByID map[uuid.UUID]*Stub
}

// LoadDirectory returns a roam directory for the path given.
func LoadDirectory(ctx Context, root string) (*Directory, error) {
	log.Infofln(ctx, "loading roam files found in the root of %s", root)
	infos, err := afero.ReadDir(ctx.FS(), root)
	if err != nil {
		return nil, err
	}

	dir := Directory{make([]Stub, 0, len(infos)), make(map[uuid.UUID]*Stub, len(infos))}
	for _, info := range infos {
		// Skip non-org and inbox files, and journal files
		path := filepath.Join(root, info.Name())
		if filepath.Ext(path) != ".org" || strings.HasPrefix(info.Name(), "inbox") {
			log.Debugfln(ctx, "skipping %s", info.Name())
			continue
		}

		if err := dir.loadFile(ctx, path); err != nil {
			return nil, err
		}
		log.Debugfln(ctx, "loaded %s", info.Name())
	}

	return &dir, nil
}

func (dir *Directory) loadFile(ctx Context, path string) error {
	doc, err := readOrgFile(ctx, path)
	if err != nil {
		return err
	}
	stub, err := ParseStub(ctx, *doc)
	if err != nil {
		return err
	}

	dir.Stubs = append(dir.Stubs, *stub)
	dir.stubsByID[stub.ID] = &dir.Stubs[len(dir.Stubs)-1]
	return nil
}

// Stub returns the stub with the given ID.
func (dir Directory) Stub(id uuid.UUID) (*Stub, bool) {
	stub, ok := dir.stubsByID[id]
	return stub, ok
}
