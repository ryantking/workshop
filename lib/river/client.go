package river

import (
	"log"
)

// Client is able to send commands to river.
type Client struct {
	Runner
}

// NewClient returns a new river Client.
func NewClient(runner Runner) Client {
	return Client{runner}
}

// DefaultClient returns a client configured with the default runner.
func DefaultClient() Client {
	return Client{NewExecRunner()}
}

// Bind creates a new keybinding.
func (c Client) Bind(mods Mods, cmd string, args ...string) error {
	args = append([]string{"map", "normal", mods.Mods(), cmd}, args...)
	return c.Run("riverctl", args...)
}

// BindF creaets a new binding and fatally logs any error.
func (c Client) BindF(mods Mods, cmd string, args ...string) {
	if err := c.Bind(mods, cmd, args...); err != nil {
		log.Fatalf("bind error: %s", err.Error())
	}
}

// BindComplex creates a new keybinding that runs a script.
func (c Client) BindScript(mods Mods, script string) error {
	return c.Run("riverctl", "map", "normal", mods.Mods(), "bash", "-c", script)
}

// BindF creaets a new script binding and fatally logs any error.
func (c Client) BindScriptF(mods Mods, script string) {
	if err := c.BindScript(mods, script); err != nil {
		log.Fatalf("bind error: %s", err.Error())
	}
}
