package log

import (
	"io"

	"github.com/pterm/pterm"
	"github.com/ryantking/workshop/lib/cli"
)

func debug(ctx cli.Context) *pterm.PrefixPrinter {
	debug := pterm.Warning.WithWriter(io.Discard).WithLineNumberOffset(1).WithShowLineNumber(true)
	if ctx.Debug() {
		debug = debug.WithWriter(ctx.ErrOut())
	}

	return debug
}

// Debug prints out values to stdout.
func Debug(ctx cli.Context, a ...interface{}) {
	debug(ctx).Print(a...)
}

// Debugln prints out debug text with a newline.
func Debugln(ctx cli.Context, s string) {
	debug(ctx).Println(s)
}

// Debugln prints out a format string if debugging is enabled.
func Debugf(ctx cli.Context, format string, a ...interface{}) {
	debug(ctx).Printf(format, a...)
}

// Debugfln prints out a format string with a newline if debugging is enabled.
func Debugfln(ctx cli.Context, format string, a ...interface{}) {
	debug(ctx).Printfln(format, a...)
}

func info(ctx cli.Context) *pterm.PrefixPrinter {
	info := pterm.Warning.WithWriter(ctx.Out()).WithLineNumberOffset(1)
	if ctx.Debug() {
		info = info.WithShowLineNumber(true)
	}

	return info
}

// Info prints out values to stdout.
func Info(ctx cli.Context, a ...interface{}) {
	info(ctx).Print(a...)
}

// Infoln prints out info text with a newline.
func Infoln(ctx cli.Context, s string) {
	info(ctx).Println(s)
}

// Infoln prints out a format string.
func Infof(ctx cli.Context, format string, a ...interface{}) {
	info(ctx).Printf(format, a...)
}

// Infofln prints out a format string with a newline if.
func Infofln(ctx cli.Context, format string, a ...interface{}) {
	info(ctx).Printfln(format, a...)
}


func warn(ctx cli.Context) *pterm.PrefixPrinter {
	warn := pterm.Warning.WithWriter(ctx.ErrOut()).WithLineNumberOffset(1)
	if ctx.Debug() {
		warn = warn.WithShowLineNumber(true)
	}

	return warn
}

// Warn prints out values to stdout.
func Warn(ctx cli.Context, a ...interface{}) {
	warn(ctx).Print(a...)
}

// Warnln prints out warn text with a newline.
func Warnln(ctx cli.Context, s string) {
	warn(ctx).Println(s)
}

// Warnln prints out a format string.
func Warnf(ctx cli.Context, format string, a ...interface{}) {
	warn(ctx).Printf(format, a...)
}

// Warnfln prints out a format string with a newline.
func Warnfln(ctx cli.Context, format string, a ...interface{}) {
	warn(ctx).Printfln(format, a...)
}

func error(ctx cli.Context) *pterm.PrefixPrinter {
	error := pterm.Error.WithWriter(ctx.ErrOut()).WithLineNumberOffset(1)
	if ctx.Debug() {
		error = error.WithShowLineNumber(true)
	}

	return error
}

// Error prints out values to stdout.
func Error(ctx cli.Context, a ...interface{}) {
	error(ctx).Print(a...)
}

// Errorln prints out error text with a newline.
func Errorln(ctx cli.Context, s string) {
	error(ctx).Println(s)
}

// Errorln prints out a format string.
func Errorf(ctx cli.Context, format string, a ...interface{}) {
	error(ctx).Printf(format, a...)
}

// Errorfln prints out a format string with a newline.
func Errorfln(ctx cli.Context, format string, a ...interface{}) {
	error(ctx).Printfln(format, a...)
}

func fatal(ctx cli.Context) *pterm.PrefixPrinter {
	fatal := pterm.Fatal.WithWriter(ctx.ErrOut()).WithLineNumberOffset(1)
	if ctx.Debug() {
		fatal = fatal.WithShowLineNumber(true)
	}

	return fatal
}

// Fatal prints out values to stdout and exits with an error code.
func Fatal(ctx cli.Context, a ...interface{}) {
	fatal(ctx).Print(a...)
}

// Fatalln prints out fatal text with a newline and exits with an error code.
func Fatalln(ctx cli.Context, s string) {
	fatal(ctx).Println(s)
}

// Fatalln prints out a format string and exits with an error code.
func Fatalf(ctx cli.Context, format string, a ...interface{}) {
	fatal(ctx).Printf(format, a...)
}

// Fatalfln prints out a format string with a newline and exits with an error code.
func Fatalfln(ctx cli.Context, format string, a ...interface{}) {
	fatal(ctx).Printfln(format, a...)
}
