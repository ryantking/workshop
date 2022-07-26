package yabai

//go:generate go run github.com/mailru/easyjson/easyjson -all display.go

// Display is a connected displays.
type Display struct {
	Index  int   `json:"index"`
	Spaces []int `json:"spaces"`
}

// Displays returns all displays known to the system.
func Displays() ([]Display, error) {
	return Query[Display]("--displays")
}

// FocusDisplay focuses the selected display.
func FocusDisplay(sel string) error {
	return Do("-m", "display", "--focus", sel)
}
