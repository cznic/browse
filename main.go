// Copyright 2016 The Browse Authors. All rights reserved.
// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.

// Command browse is a terminal Go source code browser.
//
// Usage:
//
//	browse [options] [import-path]
//
//	-n
//		print the list of files the package consists of and exit.
//
//	-tags 'tag list'
//		a list of build tags to consider satisfied during the build.
//		For more information about build tags, see the description of
//		build constraints in the documentation for the go/build
//		package.
//
// The optional argument must be a valid import path. If no argument is
// provided browse checks if the current working directory is in a subtree
// rooted in GOPATH/src, in which case it opens the package files in the
// current directory.
package main

import (
	"flag"
	"fmt"
	"log"
	"os"
	"path/filepath"
	"runtime"
	"strings"

	"github.com/cznic/browse/internal/gc"
	"github.com/cznic/wm"
	"github.com/gdamore/tcell"
)

var (
	windowStyle = wm.WindowStyle{
		Border:     wm.Style{Background: tcell.ColorSilver, Foreground: tcell.ColorNavy},
		ClientArea: wm.Style{Background: tcell.ColorSilver, Foreground: tcell.ColorNavy},
		Title:      wm.Style{Background: tcell.ColorNavy, Foreground: tcell.ColorSilver},
	}

	theme = &wm.Theme{
		ChildWindow: windowStyle,
		Desktop: wm.WindowStyle{
			ClientArea: wm.Style{Background: tcell.ColorTeal, Foreground: tcell.ColorWhite},
		},
	}
)

func env(key, default_ string) string {
	if s := os.Getenv(key); s != "" {
		return s
	}

	return default_
}

func main() {
	log.SetFlags(log.Lshortfile)
	oN := flag.Bool("n", false, `
	print the list of files the package consists of and exit.`)
	oTags := flag.String("tags", "", strings.TrimSpace(`
	a list of build tags to consider satisfied during the build. For more
	information about build tags, see the description of build constraints
	in the documentation for the go/build package.
`))
	flag.Usage = func() {
		fmt.Fprintf(os.Stderr,
			`%s [options] [import-path]

`, os.Args[0])
		flag.PrintDefaults()
	}
	flag.Parse()

	var importPath string
outer:
	switch flag.NArg() {
	case 0:
		cwd, err := os.Getwd()
		if err != nil {
			log.Fatal(err)
		}

		for _, v := range strings.Split(os.Getenv("GOPATH"), string(os.PathListSeparator)) {
			if importPath, err = filepath.Rel(filepath.Join(v, "src"), cwd); err == nil && !strings.HasPrefix(importPath, ".") {
				break outer
			}
		}

		log.Fatal("current directory is not in a subtree rooted in GOPATH/src and no import-path argument provided")
	case 1:
		importPath = flag.Arg(0)
	default:
		log.Fatal("expected at most one import-path argument")
	}

	searchPaths := append([]string{env("GOROOT", runtime.GOROOT())}, strings.Split(env("GOPATH", ""), string(os.PathListSeparator))...)
	for i, v := range searchPaths {
		searchPaths[i] = filepath.Join(v, "src")
	}
	ctx, err := gc.NewContext(
		env("GOOS", runtime.GOOS),
		env("GOARCH", runtime.GOARCH),
		strings.Fields(*oTags),
		searchPaths,
	)
	if err != nil {
		log.Fatal(err)
	}

	pkg, err := ctx.Load(importPath)
	if err != nil {
		log.Fatal(err)
	}

	if *oN {
		for _, v := range pkg.SourceFiles {
			fmt.Println(v.Path)
		}
		return
	}

	newBrowser(ctx, pkg)
}

type browser struct {
	app       *wm.Application
	newWinPos wm.Position
	packages  map[string]*gc.Package
	windows   map[string]*sfWindow
}

func newBrowser(ctx *gc.Context, pkg *gc.Package) {
	app, err := wm.NewApplication(theme)
	if err != nil {
		log.Fatal(err)
	}

	b := &browser{
		app:      app,
		packages: map[string]*gc.Package{pkg.ImportPath: pkg},
		windows:  map[string]*sfWindow{},
	}
	if err := b.app.Run(b.setup); err != nil {
		log.Fatal(err)
	}
}

func (b *browser) onKeyHandler(w *wm.Window, prev wm.OnKeyHandler, key tcell.Key, mod tcell.ModMask, r rune) bool {
	if prev != nil && prev(w, nil, key, mod, r) {
		return true
	}

	switch key {
	case tcell.KeyCtrlQ:
		b.app.Exit(nil)
		return true
	}

	return false
}

func (b *browser) setup() {
	app := b.app
	app.SetDoubleClickDuration(0)
	app.SetDesktop(app.NewDesktop())
	app.OnKey(b.onKeyHandler, nil)
}
