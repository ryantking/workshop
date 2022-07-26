package main

import (
	"fmt"
	"os"

	"github.com/ryantking/workshop/lib/cli"
	"github.com/ryantking/workshop/lib/cli/log"
	"github.com/ryantking/workshop/src/yabaictl/yabai"
)

func main() {
	ctx := cli.DefaultContext()
	if len(os.Args) == 1 {
		log.Fatalln(ctx, "expected a command")
	}

	var err error
	switch os.Args[1] {
	case "focus-next-window":
		err = yabai.FocusNextWindow()
	case "focus-previous-window":
		err = yabai.FocusPrevWindow()
	case "swap-next-window":
		err = yabai.SwapNextWindow()
	case "swap-previous-window":
		err = yabai.SwapPrevWindow()
	case "rotate-windows-clockwise":
		err = yabai.RotateWindowsClockwise()
	case "focus-space":
		if len(os.Args) < 3 {
			err = fmt.Errorf("%s space expects a selector", os.Args[0])
		} else {
			err = yabai.FocusSpace(os.Args[2])
		}
	case "set-window-space":
		if len(os.Args) < 3 {
			err = fmt.Errorf("%s set-window-space expects a selector", os.Args[0])
		}
		err = yabai.SetWindowSpace(os.Args[2])
	default:
		err = fmt.Errorf("unknown command: %s", os.Args[1])
	}
	if err != nil {
		log.Fatalln(ctx, err.Error())
	}
}
