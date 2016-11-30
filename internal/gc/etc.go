// Copyright 2016 The GC Authors. All rights reserved.
// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.

package gc

import (
	"bytes"
	"go/scanner"
	"go/token"
	"os"
	"path/filepath"
	"sync"
)

type errorList struct {
	limit int
	list  scanner.ErrorList
	mu    sync.Mutex
}

func newErrorList(limit int) *errorList {
	return &errorList{limit: limit}
}

func (l *errorList) Add(pos token.Position, msg string) {
	l.mu.Lock()
	if l.limit <= 0 {
		l.mu.Unlock()
		panic(nil)
	}

	l.limit--
	l.list.Add(pos, msg)
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

func noPos() (r token.Position) { return r }

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
