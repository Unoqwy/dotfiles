package main

import (
	"log"
	"os"

	"github.com/urfave/cli/v2"
)

func main() {
	app := &cli.App{
		Name:  "qwmhctl",
		Usage: "QWMH Control Tool (expose custom xmonad workspace features)",
		Commands: []*cli.Command{
			{
				Name:  "getworkspaces",
				Usage: "list workspaces (JSON)",
				Flags: []cli.Flag{
					&cli.BoolFlag{
						Name:    "listen",
						Aliases: []string{"l"},
						Value:   false,
						Usage:   "continuously listen for workspaces changes and print new state every time it updates",
					},
				},
				Action: func(ctx *cli.Context) error {
					return CmdGetWorkspaces(ctx.Bool("listen"))
				},
			},
		},
	}

	if err := app.Run(os.Args); err != nil {
		log.Fatal(err)
	}
}
