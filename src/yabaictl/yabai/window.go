package yabai

import (
	"fmt"
)

//go:generate go run github.com/mailru/easyjson/easyjson -all window.go

// Window is an open window.
type Window struct {
	App                string `json:"app"`
	Title              string `json:"title"`
	Display            int    `json:"display"`
	Space              int    `json:"space"`
	SplitType          string `json:"split-type"`
	StackIndex         int    `json:"stack-index"`
	HasFocus           bool   `json:"has-focus"`
	IsVisible          bool   `json:"is-visible"`
	IsNativeFullScreen bool   `json:"is-native-fullscreen"`
	IsMinimized        bool   `json:"is-minimized"`
}

// Windows returns all windows open on the current space.
func Windows() ([]Window, error) {
	return Query[Window]("--windows", "")
}

// FocusWindow focuses a window.
func FocusWindow(sel string) error {
	return Do("-m", "window", "--focus", sel)
}

// SwapWindow swaps the focused window with the selected one.
func SwapWindow(sel string) error {
	return Do("-m", "window", "--swap", sel)
}

// FocusNextWindow moves to the next window.
func FocusNextWindow() error {
	if err := FocusWindow("next"); err == nil {
		return nil
	}
	if err := FocusDisplay("next"); err != nil {
		return fmt.Errorf("no next window")
	}

	return FocusWindow("first")
}

// FocusPrevWindow moves to the previous window.
func FocusPrevWindow() error {
	if err := FocusWindow("prev"); err == nil {
		return nil
	}
	if err := FocusDisplay("prev"); err != nil {
		return fmt.Errorf("no previous window")
	}

	return FocusWindow("last")
}

// SwapNextWindow swaps the window with the next one.
//
// On space boundary, it moves it to the next monitor.
func SwapNextWindow() error {
	if err := SwapWindow("next"); err == nil {
		return nil
	}
	if err := Do("-m", "window", "--display", "next"); err != nil {
		return err
	}

	return FocusDisplay("next")
}

// SwapPrevWindow swaps the window with the previous one.
//
// On space boundary, it moves it to the previous monitor.
func SwapPrevWindow() error {
	if err := SwapWindow("prev"); err == nil {
		return nil
	}
	if err := Do("-m", "window", "--display", "prev"); err != nil {
		return err
	}

	return FocusDisplay("prev")
}

// SetWindowSpace sets the space of the current window.
func SetWindowSpace(sel string) error {
	if err := Do("-m", "window", "--space", sel); err != nil {
		return err
	}
	if err := FocusSpace(sel); err != nil {
		return err
	}

	return Do("-m", "window", "--focus", "last")
}

// RotateWindowsClockwise rotates tHe current window to the next monitor.
func RotateWindowsClockwise() error {
	if err := Do("-m", "window", "--display", "next"); err != nil {
		return err
	}
	if err := Do("-m", "display", "--focus", "next"); err != nil {
		return err
	}
	if err := Do("-m", "window", "--focus", "last"); err != nil {
		return err
	}
	if err := Do("-m", "window", "--display", "first"); err != nil {
		return err
	}
	if err := Do("-m", "display", "--focus", "first"); err != nil {
		return err
	}

	return Do("-m", "window", "--focus", "unrafirst")
}
