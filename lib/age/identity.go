package age

import (
	"fmt"
	"os"

	"filippo.io/age"
	"filippo.io/age/agessh"
	"github.com/spf13/afero"
)

// Identity is an age identty that can encrypt and decrypt files.
type Identity interface {
	age.Identity

	// Recipient returns the recipient to encrypt files for.
	Recipient() age.Recipient
}

// ParseIdentity returns an age identity from an ssh key path.
func ParseIdentity(ctx Context, sshKeyPath string) (Identity, error) {
	pemBytes, err := afero.ReadFile(ctx.FS(), os.ExpandEnv(sshKeyPath))
	if err != nil {
		return nil, err
	}
	ageId, err := agessh.ParseIdentity(pemBytes)
	if err != nil {
		return nil, fmt.Errorf("unable to parse SSH key file: %w", err)
	}
	id, ok := ageId.(Identity)
	if !ok {
		return nil, fmt.Errorf("underlying key type has no recipient: %T", ageId)
	}

	return id, nil
}
