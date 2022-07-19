package river

import (
	"io"
	"os"
	"os/exec"
)

// Runner runs river commands.
type Runner interface {
	Run(name string, args ...string) error
}

// ExecRunner runs river commands by executing them on the system.
type ExecRunner struct {
	stdin  io.Reader
	stdout io.Writer
	stderr io.Writer
}

// NexExecRunner creates a new ExecRunner, applies the options to it, and then returns it.
func NewExecRunner(opts ...func(*ExecRunner)) ExecRunner {
	runner := ExecRunner{
		stdin:  os.Stdin,
		stdout: os.Stdout,
		stderr: os.Stderr,
	}

	for _, opt := range opts {
		opt(&runner)
	}

	return runner
}

// Run executes the command on the system.
func (r ExecRunner) Run(name string, args ...string) error {
	cmd := exec.Command(name, args...)
	cmd.Stdin = r.stdin
	cmd.Stdout = r.stdout
	cmd.Stderr = r.stderr
	return cmd.Run()
}
