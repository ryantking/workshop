package refresh

import (
	"github.com/ryantking/workshop/src/calsync"
	"github.com/ryantking/workshop/src/calsync/google"
	"github.com/spf13/viper"
)

// Context contains the background values required to do refreshes.
type Context interface {
	calsync.Context

	// Client returns the client to do google things with.
	Client() google.Client
}

type context struct {
	calsync.Context
}

// NewContext returns a new context that reads from the viper state.
func NewContext() Context {
	return context{calsync.NewContext()}
}

// Client satisfies the Context interface.
func (context) Client() google.Client {
	return viper.Get("refresh.client").(google.Client)
}
