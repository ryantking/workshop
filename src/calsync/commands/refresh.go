package commands

import (
	"github.com/ryantking/workshop/src/pkg/pprint"
	"github.com/ryantking/workshop/src/projects/calsync"
	"github.com/ryantking/workshop/src/calsync/google"
	"github.com/spf13/cobra"
)

// NewRefreshCommand returns a new refersh command.
func NewRefreshCommand() *cobra.Command {
	rootCmd := cobra.Command{
		Use: "refresh NAME",
		Short: "Refresh an account",
		SilenceErrors: true,
		SilenceUsage: true,
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
		},
	}

	// client, err := google.NewClient(ctx)
	// if err != nil {
	// 	return err
	// }
	// 		for _, name := range names {
	// 			if err := Refresh(ctx, name); err != nil {
	// 				pprint.Error(ctx, "error refreshing: %s: %s", name, err.Error())
	// 			}
	// 		}
	// 	},
	// }

	return &rootCmd
}

// List lists all the accounts configured.
func Refresh(ctx calsync.Context, client google.Client, name string) error {
	_ = client

	return nil
}
