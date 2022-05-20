package calsync

import (
	"io"
	"os"

	"github.com/ryantking/workshop/lib/cli"
	"github.com/ryantking/workshop/src/calsync/google"
	"github.com/spf13/viper"
)

// Context contains the values required to operate calsync.
type Context interface {
	cli.Context
	google.Context
}

type context struct {
	google.Context
}

// NewContext returns a new context.
func NewContext() Context {
	return context{google.NewContext()}
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
