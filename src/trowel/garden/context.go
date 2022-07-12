package garden

import (
	"os"

	"github.com/ryantking/workshop/src/trowel/roam"
	"github.com/spf13/viper"
)

// Context represents values needed by the garden.
type Context interface {
	roam.Context

	// GardenDir is the directory that the garden is planted in.
	GardenDir() string
}

type context struct {
	roam.Context
}

// NewContext returns a new garden context.
func NewContext() Context {
	return context{roam.NewContext()}
}

// GardenDir satisfies the Context interface.
func (ctx context) GardenDir() string {
	return os.ExpandEnv(viper.GetString("garden-dir"))
}
