package river

import "strings"

// Mods represents a modifier combination.
type Mods interface {
	// Mods returns the string representation of all mods.
	Mods() string
}

// Mod is a symbol that represents a single modifier key.
type Mod string

var (
	Super Mod = "Super"
	Shift Mod = "Shift"
	Ctrl  Mod = "Control"
	Alt   Mod = "Alt"
)

// Mods returns the single modifier key.
func (m Mod) Mods() string {
	return string(m)
}

// ModList represents a list of modifier keys pressed at once.
type ModList []Mod

var (
	SuperShift        = ModList{Super, Shift}
	SuperAlt          = ModList{Super, Alt}
	SuperAltShift     = ModList{Super, Alt, Shift}
	SuperAltShiftCtrl = ModList{Super, Alt, Shift, Ctrl}
)

// Mods returns the list of mods as a string.
func (ml ModList) Mods() string {
	mods := make([]string, len(ml))
	for i, mod := range ml {
		mods[i] = mod.Mods()
	}

	return strings.Join(mods, "+")
}
