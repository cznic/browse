// Copyright 2016 The Browse Authors. All rights reserved.
// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.

// Command browse is a source code viewer.
//
// Usage:
//
//	browse [import-path]
//
// The optional argument must be a valid import path. If no argument is
// provided browse checks if the current working directory is in a subtree
// rooted at GOPATH/src, in which case it opens the package files in the
// current directory.
//
// Key bindings
//
// Currently all key bindings are hard coded.
//
// Application key bindings
//
// Application key bindings are processed regardless of which window, if any,
// has focus.
//
//	Ctrl-Q terminates the application.
//
// File window key bindings
//
// File windows key bindings are processed by the focused window, if any.
//
//	Ctrl-D		scrolls half page downwards in the buffer.
//	Ctrl-E		scrolls one line downwards in the buffer.
//	Ctrl-U		scrolls half page upwards in the buffer.
//	Ctrl-W		closes the window.
//	Ctrl-Y		scrolls one line upwards in the buffer.
//	End		sets the view to end at the last line.
//	Home		sets the view to begin at the first line.
//	PageDown	scrolls the view to the next page.
//	PageUp		scrolls the view to the previous page.
package main

import (
	"flag"
	"log"
	"os"
	"path/filepath"
	"runtime"
	"strings"

	"github.com/cznic/browse/internal/gc"
	"github.com/cznic/wm"
)

var app *wm.Application

func env(key, default_ string) string {
	if s := os.Getenv(key); s != "" {
		return s
	}

	return default_
}

func main() {
	log.SetFlags(log.Lshortfile)
	flag.Parse()
	wd, err := os.Getwd()
	if err != nil {
		log.Fatal(err)
	}

	searchPaths := append(
		[]string{env("GOROOT", runtime.GOROOT())},
		strings.Split(env("GOPATH", ""), string(os.PathListSeparator))...,
	)
	for i, v := range searchPaths {
		searchPaths[i] = filepath.Join(v, "src")
	}

	var importPath string
outer:
	switch flag.NArg() {
	case 0:
		for _, v := range searchPaths {
			if importPath, err = filepath.Rel(v, wd); err == nil && !strings.HasPrefix(importPath, ".") {
				break outer
			}
		}

		log.Fatal("current directory is not in a subtree rooted at {GOROOT,GOPATH}/src and no import-path argument provided")
	case 1:
		importPath = flag.Arg(0)
	default:
		log.Fatal("expected at most one import-path argument")
	}

	ctx, err := gc.NewContext(
		env("GOOS", runtime.GOOS),
		env("GOARCH", runtime.GOARCH),
		gc.VersionTags(),
		searchPaths,
		gc.DeclarationXref(),
		gc.IgnoreRedeclarations(),
	)
	if err != nil {
		log.Fatal(err)
	}

	pkg, err := ctx.Load(importPath)
	if err != nil {
		log.Fatal(err)
	}

	if err := newBrowser(ctx).run(pkg); err != nil {
		log.Fatal(err)
	}
}
