package config

import (
	_ "github.com/mailru/easyjson"
	"github.com/spf13/viper"
	"gopkg.in/yaml.v3"
)

//go:generate go run github.com/mailru/easyjson/easyjson -all config.go

// Global is the configuration for the entire daemon.
type Global struct {
	// Accounts are all of the calendar accounts to sync.
	Accounts []Account `yaml:"accounts"`

	// CalendarDir is the path to the base directory to store calendars in.
	CalendarDir string `mapstructure:"calendar_dir" yaml:"calendarDir"`

	// TokenDir is the path to store account tokens
	TokenDir string `mapstructure:"token_dir"`
}

// Account configures a google account.
type Account struct {
	// Name is the human-readable name of the account.
	Name string `json:"name"`

	// Calendars is the list of calendars to pull from the account.
	Calendars []Calendar `json:"calendars"`

	// Credentials is credentials JSON file downloaded from Google.
	Credentials map[string]interface{} `json:"credentials"`
}

// Calendar configures a google calendar.
type Calendar struct {
	// Name is the name of the calendar in Google.
	// Likely an email address.
	Name string `json:"name"`

	// Path is the path to org file to store calendar data.
	Path string `json:"path"`
}

// Save updaets the accounts in the configuration file.
func Save(ctx Context, cfg Global) error {
	f, err := ctx.FS().Create(viper.ConfigFileUsed())
	if err != nil {
		return err
	}
	defer f.Close()
	return yaml.NewEncoder(f).Encode(cfg)
}
