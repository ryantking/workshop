package color

import (
	"fmt"
	"net/http"
	"strings"

	"gopkg.in/yaml.v3"
)

// Scheme is a base16 colorscheme.
type Scheme struct {
	Name   string `yaml:"name"`
	Author string `yaml:"author"`
	Base00 string `yaml:"base00"`
	Base01 string `yaml:"base01"`
	Base02 string `yaml:"base02"`
	Base03 string `yaml:"base03"`
	Base04 string `yaml:"base04"`
	Base05 string `yaml:"base05"`
	Base06 string `yaml:"base06"`
	Base07 string `yaml:"base07"`
	Base08 string `yaml:"base08"`
	Base09 string `yaml:"base09"`
	Base0A string `yaml:"base0A"`
	Base0B string `yaml:"base0B"`
	Base0C string `yaml:"base0C"`
	Base0D string `yaml:"base0D"`
	Base0E string `yaml:"base0E"`
	Base0F string `yaml:"base0F"`
}

// LoadScheme loads the base16 scheme with the name.
func LoadScheme(name string) (*Scheme, error) {
	const baseURL = "https://raw.githubusercontent.com/base16-project/base16-schemes/main/"
	res, err := http.Get(baseURL + strings.ToLower(name) + ".yaml")
	if err != nil {
		return nil, err
	}
	defer res.Body.Close()
	if res.StatusCode != http.StatusOK {
		return nil, fmt.Errorf("bad respons: %s", res.Status)
	}
	var scheme Scheme
	if err := yaml.NewDecoder(res.Body).Decode(&scheme); err != nil {
		return nil, err
	}

	return &scheme, nil
}
