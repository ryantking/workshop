package commands

import (
	"encoding/json"

	"github.com/ryantking/workshop/lib/cli/log"
	"github.com/ryantking/workshop/src/calsync"
	"github.com/ryantking/workshop/src/calsync/config"
	"github.com/spf13/cobra"
)

// NewAddCommand returns a new command to add an account.
func NewAddCommand() *cobra.Command {
	return &cobra.Command{
		Use:           "add NAME CREDENTIALS_PATH",
		Short:         "Add an account to calsync",
		SilenceErrors: true,
		SilenceUsage:  true,
		Args:          cobra.ExactArgs(2),
		Run: func(cmd *cobra.Command, args []string) {
			ctx := calsync.NewContext()
			if err := Add(ctx, args[0], args[1]); err != nil {
				log.Fatalln(ctx, err.Error())
			}
		},
	}
}

// Add adds a new account with the given name.
func Add(ctx calsync.Context, name, credsPath string) error {
	account := config.Account{
		Name:        name,
		Calendars:   make([]config.Calendar, 0),
		Credentials: make(map[string]interface{}),
	}
	f, err := ctx.FS().Open(credsPath)
	if err != nil {
		return err
	}
	defer f.Close()
	if err := json.NewDecoder(f).Decode(&account.Credentials); err != nil {
		return err
	}
	cfg := ctx.Config()
	cfg.Accounts = append(cfg.Accounts, account)
	return config.Save(ctx, cfg)
}
