package google

import (
	"encoding/json"
	"fmt"
	"io/fs"
	"path/filepath"
	"strings"

	"github.com/ryantking/workshop/lib/age"
	"github.com/ryantking/workshop/src/calsync/config"
	"github.com/spf13/afero"
	"github.com/spf13/viper"
	"golang.org/x/oauth2"
)

// Context contains the values configured elsewhere that configure working with a google API.
type Context interface {
	config.Context

	// Identity contains the age identity for encrypting/decrypting data.
	Identity() age.Identity
}

type context struct {
	config.Context
}

// NewContext returns a new calsync context with default values.
func NewContext() Context {
	return context{config.NewContext()}
}

// Identity satisfies the Context interface
func (ctx context) Identity() age.Identity {
	return viper.Get("id").(age.Identity)
}

// Client performs actions against the google calendar API.
type Client struct {
	tokens map[string]oauth2.Token
}

// NewClient returns a new client and reads all existing data from the path, if any.
func NewClient(ctx Context) (*Client, error) {
	client := Client{make(map[string]oauth2.Token)}
	if err := client.loadTokens(ctx); err != nil {
		return nil, err
	}

	return &client, nil
}

func (c *Client) loadTokens(ctx Context) error {
	fsys := age.NewFs(ctx, ctx.Identity())
	fmt.Println(ctx.TokenDir())
	return afero.Walk(fsys, ctx.TokenDir(), func(path string, info fs.FileInfo, err error) error {
		if err != nil {
			return err
		}
		if info.IsDir() {
			return nil
		}
		f, err := fsys.Open(path)
		if err != nil {
			return err
		}
		defer f.Close()
		var token oauth2.Token
		if err := json.NewDecoder(f).Decode(&token); err != nil {
			return err
		}

		name := strings.TrimSuffix(filepath.Base(path), ".json.age")
		c.tokens[name] = token
		return nil
	})
}

func (c Client) Refresh(ctx Context, account config.Account) error {
	return nil
}

func (c Client) Save(ctx Context) error {
	return nil
}
