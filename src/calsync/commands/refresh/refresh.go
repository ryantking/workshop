package refresh

import (
	"fmt"

	"github.com/ryantking/workshop/lib/cli/log"
	"github.com/ryantking/workshop/src/calsync"
	"github.com/ryantking/workshop/src/calsync/google"
	"github.com/spf13/cobra"
	"github.com/spf13/viper"
)

// NewCommand returns a new refersh command.
func NewCommand() *cobra.Command {
	rootCmd := cobra.Command{
		Use:           "refresh NAME",
		Short:         "Refresh an account",
		SilenceErrors: true,
		SilenceUsage:  true,
		PreRun: func(cmd *cobra.Command, args []string) {
			ctx := calsync.NewContext()
			client, err := google.NewClient(ctx)
			if err != nil {
				log.Fatal(ctx, "could not setup google client: %s", err.Error())
			}
			viper.Set("refresh.client", client)
		},
		Run: func(cmd *cobra.Command, args []string) {
			ctx := calsync.NewContext()
			names := args
			if len(names) == 0 {
				accounts := ctx.Accounts()
				names = make([]string, len(accounts))
				for i, account := range accounts {
					names[i] = account.Name
				}
			}

			for _, name := range names {
				if err := Refresh(ctx, name); err != nil {
					log.Error(ctx, "error refreshing: %s: %s", name, err.Error())
				}
			}
		},
	}

	return &rootCmd
}

// List lists all the accounts configured.
func Refresh(ctx calsync.Context, name string) error {
	fmt.Println("refreshing " + name)
	return nil
}
