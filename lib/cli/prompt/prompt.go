package prompt

import (
	"bufio"
	"fmt"
	"strings"

	"github.com/ryantking/workshop/lib/cli"
	"github.com/ryantking/workshop/lib/cli/log"
)

// ForString prompts the user for a string.
func ForString(ctx cli.Context, prompt string) string {
	r := bufio.NewReader(ctx.In())
	for {
		fmt.Fprint(ctx.Out(), prompt+" ")
		s, err := r.ReadString('\n')
		if err != nil {
			log.Errorfln(ctx, "unable to read input: %s", err.Error())
			continue
		}

		return strings.TrimSpace(s)
	}
}
