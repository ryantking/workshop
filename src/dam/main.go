package main

import (
	"fmt"
	"os"
	"os/exec"
	"path/filepath"

	"github.com/nightlyone/lockfile"
	"github.com/ryantking/workshop/src/dam/river"
	"github.com/zerodha/logf"
)

var logger = logf.New(logf.Opts{})

type config struct {
	kanshiPath   string
	swaylockPath string
	swayidlePath string
	swaybgPath   string
	wofiPath     string
}

func loadConfig() config {
	return config{
		kanshiPath:   getenv("KANSHI_PATH", "kanshi"),
		swaylockPath: getenv("SWAYLOCK_PATH", "swaylock"),
		swayidlePath: getenv("SWAYIDLE_PATH", "swayidle"),
		swaybgPath:   getenv("SWAYBG_PATH", "swaybg"),
		wofiPath:     getenv("WOFI_PATH", "wofi"),
	}
}

func getenv(name, def string) string {
	s := os.Getenv(name)
	if s == "" {
		return def
	}

	return s
}

func ensureUnique() error {
	lockFile, err := lockfile.New(os.TempDir() + "/dam.lock")
	if err != nil {
		return fmt.Errorf("unable to create lockfile: %w", err)
	}

	return lockFile.TryLock()
}

func unlock() {
	lockFile, err := lockfile.New(os.TempDir() + "/dam.lock")
	if err != nil {
		logger.Fatal("unable to read lockfile: " + err.Error())
	}

	if err := lockFile.Unlock(); err != nil {
		logger.Fatal("unable to unlock lockfile: " + err.Error())
	}
}

var done = func() {
	unlock()
}

func attach(name string, args ...string) error {
	logDir := filepath.Join(os.TempDir(), "dam")
	if err := os.MkdirAll(logDir, 0o755); err != nil {
		return err
	}
	logFile := filepath.Join(logDir, filepath.Base(name)+".log")
	logger.Debug("creating logfile", "file", logFile)
	out, err := os.Create(logFile)
	if err != nil {
		return err
	}
	lastDone := done
	done = func() {
		logger.Debug("closing log file", "file", out.Name())
		if err := out.Close(); err != nil {
			logger.Error("unable to close log file: "+err.Error(), "file", out.Name())
		}

		lastDone()
	}

	cmd := exec.Command(name, args...)
	cmd.Stdout = out
	cmd.Stderr = out
	logger.Info("starting process: " + cmd.String())
	return cmd.Start()
}

func main() {
	defer func() {
		done()
	}()

	logger.Level = logf.DebugLevel
	logger.Info("starting dam", "pid", os.Getpid())
	if err := do(); err != nil {
		logger.Fatal(err.Error())
	}
}

func do() error {
	if err := ensureUnique(); err != nil {
		return err
	}

	cfg := loadConfig()
	if err := attach(cfg.kanshiPath); err != nil {
		return fmt.Errorf("unable to start kanshi: %w", err)
	}

	return river.Lock()
}
