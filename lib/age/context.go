package age

import "github.com/spf13/afero"

// Context are all the values defined in the background that age operations may require.
type Context interface {
	// FS returns the underlying filesystem to read/write from.
	FS() afero.Fs
}

type context struct {
	fs afero.Fs
}

// NewContext returns a new age context.
func NewContext() Context {
	return context{afero.NewOsFs()}
}

// FS satisiefs the Context interface
func (ctx context) FS() afero.Fs {
	return ctx.fs
}
