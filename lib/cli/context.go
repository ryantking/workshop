package cli

import (
	"io"
)

// Context holds the required values to print output.
type Context interface {
	// Debug returns whether debug output is enabled
	Debug() bool

	// In returns the reader to read user input from.
	In() io.Reader

	// Out returns the writer to send normal output to.
	Out() io.Writer

	// ErrOut returns the writer to send error output to.
	ErrOut() io.Writer
}
