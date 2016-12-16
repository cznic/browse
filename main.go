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
// Navigation
//
// Keys recognized by the focused window or the application itself.
//
//	<PageUp> or CTRL-B
//		Scroll window one page Backwards (upwards) in the buffer.
//
//	CTRL-D
//		Scroll window half a screen Downwards in the buffer.
//
//	CTRL-E
//		Scroll window one line downwards in the buffer. Mnemonic: Extra lines.
//
//	<PageDown> or CTRL-F
//		Scroll window one page Forwards (downwards) in the buffer.
//
//	<Tab> or CTRL-I
//		Go to newer cursor position in location history list
//
//	CTRL-U
//		Scroll window half a screen Upwards in the buffer.
//
//	CTRL-O
//		Go to Older cursor position in location history list
//
//	CTRL-Q
//		Quit the application.
//
//	CTRL-W
//		Close the Window.
//
//	CTRL-Y
//		Scrolls one line upwards in the buffer.
//
//	<Home>
//		Scroll to the first line.
//
//	<End>
//		Scroll to the last line.
//
// Screenshot
//
// An example session (colors cannot be shown here):
//
//	TODO
//
// Known issues
//
// Key bindings and terminal colors are hard coded.
//
// In terminals with no mouse support there's no way to select and follow links
// to declarations.
//
// Identifiers in certain contexts cannot be resolved to their respective
// declarations without type information available. Browse does not (yet) type
// check the loaded packages. The (slower) fallback in such cases depends on
// the guru tool. To install guru:
//
//	go get -u golang.org/x/tools/cmd/guru
//
package main

import (
	"flag"
	"log"
	"os"
	"os/exec"
	"path/filepath"
	"runtime"
	"strings"
	"time"

	"github.com/cznic/browse/internal/gc"
	"github.com/cznic/wm"
)

var (
	app *wm.Application
)

func env(key, default_ string) string {
	if s := os.Getenv(key); s != "" {
		return s
	}

	return default_
}

func main() {
	log.SetFlags(log.Lshortfile)
	flag.Parse()
	guru, err := exec.LookPath("guru")
	if err != nil {
		log.Fatalf("%s\nPlease install the guru tool: go get -u golang.org/x/tools/cmd/guru", err)
	}

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

	defaultTags := gc.VersionTags()
	if s := env("CGO_ENABLED", "1"); s == "1" {
		defaultTags = append(defaultTags, "cgo")
	}
	ctx, err := gc.NewContext(
		env("GOOS", runtime.GOOS),
		env("GOARCH", runtime.GOARCH),
		defaultTags,
		searchPaths,
		gc.DeclarationXref(),
	)
	if err != nil {
		log.Fatal(err)
	}

	t := time.Now()
	pkg, err := ctx.Load(importPath)
	if err != nil {
		log.Fatal(err)
	}

	if debug {
		log.Printf("loaded+xref0: %v packages in %v", ctx.NumPackages(), time.Since(t))
	}

	if err := newBrowser(ctx, guru).run(pkg); err != nil {
		log.Fatal(err)
	}
}
