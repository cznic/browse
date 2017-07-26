// Copyright 2016 The GC Authors. All rights reserved.
// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.

package gc

import (
	"bytes"
	"fmt"
	"go/scanner"
	"go/token"
	"os"
	"path/filepath"
	"sync"
	"unicode"
	"unicode/utf8"
)

type errorList struct {
	limit int
	list  scanner.ErrorList
	mu    sync.Mutex
}

func newErrorList(limit int) *errorList {
	if limit == 0 {
		limit = 1
	}
	return &errorList{limit: limit}
}

func (l *errorList) Add(position token.Position, msg string) {
	l.mu.Lock()
	if l.limit == 0 {
		l.mu.Unlock()
		panic(fmt.Errorf("%s: %v", position, msg))
	}

	l.limit--
	l.list.Add(position, msg)
	l.mu.Unlock()
}

func (l *errorList) error() error {
	l.mu.Lock()
	if len(l.list) == 0 {
		l.mu.Unlock()
		return nil
	}

	l.mu.Unlock()
	return l
}

func (l *errorList) errorString() string {
	var b bytes.Buffer
	for _, v := range l.list {
		b.WriteString(v.Error())
		b.WriteByte('\n')
	}
	s := b.String()
	return s
}

func (l *errorList) Error() string {
	l.mu.Lock()
	s := l.errorString()
	l.mu.Unlock()
	return s
}

func checkDir(dir string) (bool, error) {
	fi, err := os.Stat(dir)
	if err != nil {
		if !os.IsNotExist(err) {
			return false, err
		}

		return false, nil
	}

	if !fi.IsDir() {
		return false, nil
	}

	m, err := filepath.Glob(filepath.Join(dir, "*.go"))
	return len(m) != 0, err
}

func isExported(nm string) bool {
	switch c := nm[0]; {
	case c >= 'A' && c <= 'Z':
		return true
	case c < 0x80:
		return false
	default:
		r, _ := utf8.DecodeRuneInString(nm)
		return unicode.IsUpper(r)
	}
}
