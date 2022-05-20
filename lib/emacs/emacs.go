package emacs

import (
	"os"
	"os/exec"
	"path/filepath"
)

// Eval runs an emacs lisp expression non-interactively.
func Eval(expr string, initFiles ...string) error {
	args := []string{"--batch"}
	for _, f := range initFiles {
		path, _ := filepath.Abs(f)
		args = append(args, "-l", path)
	}
	args = append(args, "--eval", expr)
	cmd := exec.Command("emacs", args...)
	cmd.Stdout = os.Stdout
	cmd.Stderr = os.Stderr
	return cmd.Run()
}
