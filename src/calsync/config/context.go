package config

import (
	libcontext "context"
	"os"

	"github.com/spf13/afero"
	"github.com/spf13/viper"
)

// Context represents all the values stored in the configuration.
type Context interface {
	libcontext.Context

	// FS returns the underlying filesystem to read/write from.
	FS() afero.Fs

	// Config returns the entire configuration as a struct.
	Config() Global

	// Accounts are all of the calendar accounts to sync.
	Accounts() []Account

	// CalendarDir is the path to the base directory to store calendars in.
	CalendarDir() string

	// TokenDir is the path to store account tokens
	TokenDir() string
}

type context struct {
	libcontext.Context
	fs afero.Fs
}

// NewContext returns a new configuration context.
func NewContext() Context {
	return context{libcontext.Background(), afero.NewOsFs()}
}

// FS returns the configured filesystem.
func (ctx context) FS() afero.Fs {
	return ctx.fs
}

// Accounts satisiefs the Context interface
func (context) Config() Global {
	var cfg Global
	if err := viper.Unmarshal(&cfg); err != nil {
		panic(err)
	}

	return cfg
}

// Accounts satisiefs the Context interface
func (context) Accounts() []Account {
	var accounts []Account
	if err := viper.UnmarshalKey("accounts", &accounts); err != nil {
		panic(err)
	}

	return accounts
}

// CalendarDir is the directory to put calendars in
func (ctx context) CalendarDir() string {
	return ctx.path("calendarDir")
}

// TokenDir is the directory to put tokens in
func (ctx context) TokenDir() string {
	return ctx.path("tokenDir")
}

func (context) path(key string) string {
	return os.ExpandEnv(viper.GetString(key))
}
