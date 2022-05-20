package roam

import (
	"io"
	"os"

	"github.com/ryantking/workshop/lib/age"
	"github.com/ryantking/workshop/lib/cli"
	"github.com/spf13/afero"
	"github.com/spf13/viper"
)

// Context contains the necessary values to manage org documents.
type Context interface {
	age.Context
	cli.Context
}

type context struct {
	fs afero.Fs
}

// NewContext returns a new roam context.
func NewContext() Context {
	return context{afero.NewOsFs()}
}

// Debug satisfies the Context interface.
func (ctx context) Debug() bool {
	return viper.GetBool("debug")
}

// In satisfies the Context interface.
func (ctx context) In() io.Reader {
	return os.Stdin
}

// Out satisfies the Context interface.
func (ctx context) Out() io.Writer {
	return os.Stdout
}

// ErrOut satisfies the Context interface.
func (ctx context) ErrOut() io.Writer {
	return os.Stderr
}

// FS satisiefs the Context interface.
func (ctx context) FS() afero.Fs {
	return ctx.fs
}
