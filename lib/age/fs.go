package age

import (
	"errors"
	"io"

	"filippo.io/age"
	"github.com/spf13/afero"
	"go.uber.org/multierr"
)

// Fs is a filesystem that reads and writes encrypted files.
// Note that files cannot be opened for both reading and writing at the same time.
type Fs struct {
	afero.Fs
	id Identity
}

// NewFs returns a new encrypted filesystem that encrypts using the provided ID.
func NewFs(ctx Context, id Identity) Fs {
	return Fs{ctx.FS(), id}
}

// Open opens aen encrypted file for reading.
func (fs Fs) Open(path string) (afero.File, error) {
	f, err := fs.Fs.Open(path)
	if err != nil {
		return nil, err
	}
	r, err := age.Decrypt(f, fs.id)
	if err != nil {
		return nil, err
	}
	return &FileReader{f, r}, nil
}

// Create creates or truncates an encrypted file.
func (fs Fs) Create(path string) (afero.File, error) {
	f, err := fs.Fs.Create(path)
	if err != nil {
		return nil, err
	}
	w, err := age.Encrypt(f, fs.id.Recipient())
	if err != nil {
		return nil, err
	}
	return &FileWriter{f, w}, nil
}

// FileWriter encrypts and writes data to a file.
type FileWriter struct {
	afero.File
	w io.WriteCloser
}

// Read errors since the file is opened for writing.
func (f FileWriter) Read(p []byte) (int, error) {
	return 0, errors.New("encrypted file can only be written to")
}

// Write encrypts and writes bytes to the underlying file.
func (f FileWriter) Write(p []byte) (int, error) {
	return f.w.Write(p)
}

// Close writes the encrypetd data to the file and closes it.
func (f FileWriter) Close() error {
	return multierr.Combine(f.w.Close(), f.File.Close())
}

// FileReader reads and decrypts data from an encrypted file.
type FileReader struct {
	afero.File
	r io.Reader
}

// Read decrypts and reads bytes.
func (f FileReader) Read(p []byte) (int, error) {
	return f.r.Read(p)
}

// Write throws an error when you write to a reader file.
func (f FileReader) Write(p []byte) (int, error) {
	return 0, errors.New("encrypted file can only be read from")
}
