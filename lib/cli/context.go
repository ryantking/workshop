package cli

import (
	libcontext "context"
	"io"
	"os"
)

// Context holds the required values to print output.
type Context interface {
	libcontext.Context

	// Debug returns whether debug output is enabled
	Debug() bool

	// In returns the reader to read user input from.
	In() io.Reader

	// Out returns the writer to send normal output to.
	Out() io.Writer

	// ErrOut returns the writer to send error output to.
	ErrOut() io.Writer
}

func DefaultContext() Context {
	return context{libcontext.Background()}
}

type context struct {
	libcontext.Context
}

func (context) Debug() bool {
	return os.Getenv("YABAICTL_DEBUG") == "1"
}

func (context) In() io.Reader     { return os.Stdin }
func (context) Out() io.Writer    { return os.Stdout }
func (context) ErrOut() io.Writer { return os.Stderr }
