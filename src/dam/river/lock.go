package river

import (
	"os"
	"os/exec"

	"github.com/ryantking/workshop/lib/color"
)

// Lock locks the current river session.
func Lock(extraArgs ...string) error {
	scheme, err := color.LoadScheme(os.Getenv("COLORSCHEME"))
	if err != nil {
		return err
	}

	swaylock := "swaylock"
	if s := os.Getenv("SWAYLOCK_PATH"); s != "" {
		swaylock = s
	}
	_ = scheme

	// TODO: Move color config to file in nix adn delete color lib
	cmd := exec.Command(swaylock, append([]string{
		// "-i", os.Getenv("WORKSHOP_DIR") + "/usr/wallpaper/nord/cpu_city.png",
		"--screenshots",
		"--effect-blur", "7x5",
		"--effect-compose", "150,-150;50x50;center;" + os.Getenv("WORKSHOP_DIR") + "/usr/icons/lock.svg",
		"--indicator-radius", "120",
		"--indicator-x-position", "200",
		"--indicator-y-position", "1960",
		"--hide-keyboard-layout",

		"--text-color", "00000000",
		"--text-clear-color", "00000000",
		"--text-caps-lock-color", "00000000",
		"--text-ver-color", "00000000",
		"--text-wrong-color", "00000000",
		"--line-color", "00000000",
		"--line-clear-color", "00000000",
		"--line-caps-lock-color", "00000000",
		"--line-ver-color", "00000000",
		"--line-wrong-color", "00000000",
		"--inside-color", "00000000",
		"--inside-clear-color", "00000000",
		"--inside-ver-color", "00000000",
		"--inside-wrong-color", "00000000",
		"--separator-color", "00000000",
		"--ring-color", scheme.Base04 + "88",
		"--ring-clear-color", scheme.Base04 + "88",
		"--ring-ver-color", scheme.Base0A + "88",
		"--ring-wrong-color", scheme.Base08 + "88",
		"--key-hl-color", scheme.Base05 + "88",
		"--bs-hl-color", scheme.Base0A + "88",
	}, extraArgs...)...)

	cmd.Stdout = os.Stdout
	cmd.Stderr = os.Stderr

	return cmd.Run()
}
