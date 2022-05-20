package trowel

import (
	"github.com/ryantking/workshop/src/trowel/garden"
)

// Context containts the global configuration values for trowel.
type Context interface {
	garden.Context
}

type context struct {
	garden.Context
}

// NewContext returns a new output context with sensible defaults.
// Out and ErrOut will be stdout and stderr respectively.
func NewContext() Context {
	return context{garden.NewContext()}
}
