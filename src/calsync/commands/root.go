package commands

import (
	"os"

	"github.com/ryantking/workshop/lib/age"
	"github.com/ryantking/workshop/lib/cli/log"
	"github.com/ryantking/workshop/src/calsync"
	"github.com/ryantking/workshop/src/calsync/commands/refresh"
	"github.com/spf13/cobra"
	"github.com/spf13/viper"
)

// Execute runs the trowel command.
func Execute() {
	var (
		ctx = calsync.NewContext()

		configPath string
		sshKeyPath string
	)

	rootCmd := cobra.Command{
		Use:           "trowel",
		Short:         "Build your digital garden",
		SilenceErrors: true,
		SilenceUsage:  true,
		PersistentPreRun: func(cmd *cobra.Command, args []string) {
			viper.SetConfigFile(os.ExpandEnv(configPath))
			viper.SetDefault("calendar_dir", "$HOME/Dropbox/org/cal")
			viper.SetDefault("token_dir", "$XDG_DATA_DIR/calsync")
			if err := viper.ReadInConfig(); err != nil && !os.IsNotExist(err) {
				log.Fatalfln(ctx, "unable to load config: %s", err.Error())
			}

			id, err := age.ParseIdentity(ctx, sshKeyPath)
			if err != nil {
				log.Fatalfln(ctx, "unable to load age identity: %s", err.Error())
			}

			viper.Set("id", id)
		},
	}

	rootCmd.PersistentFlags().BoolP("debug", "x", false, "Enable debug printing")
	rootCmd.PersistentFlags().StringVarP(
		&configPath, "config", "c",
		"$XDG_CONFIG_HOME/calsync.yaml",
		"Path to the calsync configuration file to load",
	)
	rootCmd.PersistentFlags().StringVar(&sshKeyPath, "ssh-key", "$HOME/.ssh/id_ed25519", "Path to the SSH key to use")

	viper.BindPFlag("debug", rootCmd.PersistentFlags().Lookup("debug"))

	rootCmd.AddCommand(
		NewAddCommand(),
		refresh.NewCommand(),
	)

	if err := rootCmd.Execute(); err != nil {
		log.Fatalln(ctx, err.Error())
	}
}
