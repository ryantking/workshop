package trowel

import (
	"github.com/ryantking/workshop/lib/cli/log"
	"github.com/ryantking/workshop/lib/funcy"
	"github.com/ryantking/workshop/src/trowel/garden"
	"github.com/ryantking/workshop/src/trowel/roam"
	"github.com/spf13/cobra"
	"github.com/spf13/viper"
)

// Execute runs the trowel command.
func Execute() {
	ctx := NewContext()
	rootCmd := &cobra.Command{
		Use:           "trowel",
		Short:         "Build your digital garden",
		SilenceErrors: true,
		SilenceUsage:  true,
		Args:          cobra.ExactArgs(1),
		Run: func(cmd *cobra.Command, args []string) {
			roamDir, err := roam.LoadDirectory(ctx, args[0])
			if err != nil {
				log.Fatalfln(ctx, "unable to load roam directory: %s", err)
			}
			gardener, err := garden.NewGardener(ctx, *roamDir)
			if err != nil {
				log.Fatalln(ctx, err.Error())
			}
			for _, stub := range roamDir.Stubs {
				if !funcy.Contains(stub.Tags, "public") {
					log.Debugfln(ctx, "skipping private note: %s", stub.Title)
					continue
				}

				if err := gardener.Plant(ctx, stub); err != nil {
					log.Warnfln(ctx, "Unable to plant garden: %s", err.Error())
				}
			}
		},
	}

	rootCmd.PersistentFlags().BoolP("debug", "x", false, "Enable debug printing")
	rootCmd.PersistentFlags().String("garden-dir", "$WORKSHOP_DIR/src/site", "The directory to write garden files to")
	viper.BindPFlag("debug", rootCmd.PersistentFlags().Lookup("debug"))
	viper.BindPFlag("garden-dir", rootCmd.PersistentFlags().Lookup("garden-dir"))

	if err := rootCmd.Execute(); err != nil {
		log.Fatalln(ctx, err.Error())
	}
}
