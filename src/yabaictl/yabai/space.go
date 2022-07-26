package yabai

import "fmt"

//go:generate go run github.com/mailru/easyjson/easyjson -all space.go

// Space is a connected displays.
type Space struct {
	Index              int    `json:"index"`
	Label              string `json:"label"`
	Display            int    `json:"display"`
	Windows            []int  `json:"windows"`
	FirstWindow        int    `json:"first-window"`
	LastWindow         int    `json:"last-window"`
	HasFocus           bool   `json:"has-focus"`
	IsVisible          bool   `json:"is-visible"`
	IsNativeFullScreen bool   `json:"is-native-fullscreen"`
}

// Spaces returns all spaces known to the system.
func Spaces() ([]Space, error) {
	return Query[Space]("--spaces")
}

// FocusedSpace returns the focused space.
func FocusedSpace() (*Space, error) {
	spaces, err := Spaces()
	if err != nil {
		return nil, err
	}

	for _, space := range spaces {
		if space.HasFocus {
			return &space, nil
		}
	}

	return nil, fmt.Errorf("no space is focused")
}

// FocusSpace chanegs the space on the current display.
func FocusSpace(sel string) error {
	if err := Do("-m", "space", "--focus", sel); err != nil {
		return err
	}

	return FocusWindow("first")
}

type Layout string

var (
	BSP   Layout = "layout"
	Stack Layout = "stack"
	Float Layout = "float"
)

// SetLayout sets the layout of the current space.
func SetLayout(layout Layout) error {
	return Do("--layout", string(layout))
}
