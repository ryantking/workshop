package yabai

import (
	"bytes"
	"encoding/json"
	"fmt"
	"os"
	"os/exec"
)

var yabaiPath = "yabai"

func init() {
	if s := os.Getenv("YABAI_PATH"); s != "" {
		yabaiPath = s
	}
}

// Do sends yabai a command.
func Do(args ...string) error {
	var out bytes.Buffer
	cmd := exec.Command(yabaiPath, args...)
	cmd.Stdout = &out
	cmd.Stderr = &out
	if err := cmd.Run(); err != nil {
		if out.Len() == 0 {
			return err
		}

		return fmt.Errorf("%s: %s", err.Error(), out.String())
	}

	return nil
}

// Qurey queries yabai for information and unmarshals it.
func Query[T any](args ...string) ([]T, error) {
	cmd := exec.Command(yabaiPath, append(
		[]string{"-m", "query"},
		args...,
	)...)

	stdout, err := cmd.StdoutPipe()
	if err != nil {
		return nil, err
	}
	if err := cmd.Start(); err != nil {
		return nil, err
	}

	var res []T
	if err := json.NewDecoder(stdout).Decode(&res); err != nil {
		return nil, err
	}
	if err := cmd.Wait(); err != nil {
		return nil, err
	}

	return res, nil
}
