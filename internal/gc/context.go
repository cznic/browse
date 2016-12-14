// Copyright 2016 The GC Authors. All rights reserved.
// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.

package gc

import (
	"fmt"
	"go/token"
	"os"
	"path/filepath"
	"runtime/debug"
	"strings"
	"sync"

	"github.com/edsrzf/mmap-go"
)

var (
	validOS = map[string]bool{ // Go 1.7
		"android":   true,
		"darwin":    true,
		"dragonfly": true,
		"freebsd":   true,
		"linux":     true,
		"nacl":      true,
		"netbsd":    true,
		"openbsd":   true,
		"plan9":     true,
		"solaris":   true,
		"windows":   true,
	}

	archModels = map[string]model{ // Go 1.7
		"386":         {4, 4},
		"amd64":       {8, 8},
		"amd64p32":    {8, 4},
		"arm64":       {8, 8},
		"arm64be":     {8, 8},
		"arm":         {4, 4},
		"armbe":       {4, 4},
		"mips64":      {8, 8},
		"mips64le":    {8, 8},
		"mips64p32":   {8, 4},
		"mips64p32le": {8, 4},
		"mips":        {8, 8}, //TODO ?
		"mipsle":      {8, 8}, //TODO ?
		"ppc64":       {8, 8},
		"ppc64le":     {8, 8},
		"ppc":         {4, 4},
		"s390":        {4, 4},
		"s390x":       {8, 8},
		"sparc64":     {8, 8},
		"sparc":       {4, 4},
	}
)

func isValidArch(s string) bool {
	_, ok := archModels[s]
	return ok
}

type model struct {
	intBytes int // Size of int.
	ptrBytes int // Size of *T, unsafe.Pointer and uintptr.
}

type tweaks struct {
	declarationXref      bool
	ignoreRedeclarations bool
	ignoreUndefined      bool
}

// Option amends Context.
type Option func(c *Context) error

// DeclarationXref enables keeping a declaratio cross reference.
func DeclarationXref() Option {
	return func(c *Context) error {
		c.tweaks.declarationXref = true
		return nil
	}
}

// IgnoreRedeclatations disables reporting redeclaration errors.
func IgnoreRedeclarations() Option {
	return func(c *Context) error {
		c.tweaks.ignoreRedeclarations = true
		return nil
	}
}

// IgnoreUndefined disables reporting undefined errors.
func IgnoreUndefined() Option {
	return func(c *Context) error {
		c.tweaks.ignoreUndefined = true
		return nil
	}
}

// Context describes the context of loaded packages.
type Context struct {
	FileSet       *token.FileSet // Contains all loaded files.
	goarch        string
	goos          string
	ignoreImports bool // Test hook.
	model         model
	packages      map[string]*Package // Key: import path.
	packagesMu    sync.Mutex
	searchPaths   []string
	tags          map[string]struct{}
	tweaks        tweaks
	universe      *Scope
}

// NewContext returns a newly created Context. tags are the build tags
// considered when loading packages having build directives (see
// https://golang.org/pkg/go/build/#hdr-Build_Constraints for details).
// searchPaths are examined when looking for a package to load.
func NewContext(goos, goarch string, tags, searchPaths []string, options ...Option) (*Context, error) {
	if !validOS[goos] {
		return nil, fmt.Errorf("unknown operating system: %s", goos)
	}

	model, ok := archModels[goarch]
	if !ok {
		return nil, fmt.Errorf("unknown architecture: %s", goarch)
	}

	tm := make(map[string]struct{}, len(tags))
	for _, v := range tags {
		tm[v] = struct{}{}
	}
	tm[goos] = struct{}{}
	tm[goarch] = struct{}{}
	c := &Context{
		FileSet:     token.NewFileSet(),
		goarch:      goarch,
		goos:        goos,
		model:       model,
		packages:    map[string]*Package{},
		searchPaths: append([]string(nil), searchPaths...),
		tags:        tm,
		universe:    newScope(UniverseScope, nil),
	}
	for _, o := range options {
		if err := o(c); err != nil {
			return nil, err
		}
	}
	return c, nil
}

/*

Vendor Directories

Go 1.6 includes support for using local copies of external dependencies
to satisfy imports of those dependencies, often referred to as vendoring.

Code below a directory named "vendor" is importable only
by code in the directory tree rooted at the parent of "vendor",
and only using an import path that omits the prefix up to and
including the vendor element.

Here's the example from the previous section,
but with the "internal" directory renamed to "vendor"
and a new foo/vendor/crash/bang directory added:

    /home/user/gocode/
        src/
            crash/
                bang/              (go code in package bang)
                    b.go
            foo/                   (go code in package foo)
                f.go
                bar/               (go code in package bar)
                    x.go
                vendor/
                    crash/
                        bang/      (go code in package bang)
                            b.go
                    baz/           (go code in package baz)
                        z.go
                quux/              (go code in package main)
                    y.go

The same visibility rules apply as for internal, but the code
in z.go is imported as "baz", not as "foo/vendor/baz".

Code in vendor directories deeper in the source tree shadows
code in higher directories. Within the subtree rooted at foo, an import
of "crash/bang" resolves to "foo/vendor/crash/bang", not the
top-level "crash/bang".

Code in vendor directories is not subject to import path
checking (see 'go help importpath').

When 'go get' checks out or updates a git repository, it now also
updates submodules.

Vendor directories do not affect the placement of new repositories
being checked out for the first time by 'go get': those are always
placed in the main GOPATH, never in a vendor subtree.

See https://golang.org/s/go15vendor for details.

*/
func (c *Context) dirForImportPath(importPath string) (string, error) {
	var a []string
	if importPath != "C" {
		s := importPath
		a = []string{filepath.Join(s, "vendor")}
		for s != "" {
			s = filepath.Dir(s)
			if s == "." {
				s = ""
			}
			a = append(a, filepath.Join(s, "vendor"))
		}
	}
	a = append(a, "")
	for _, v := range c.searchPaths {
		for _, w := range a {
			dir := filepath.Join(v, w, importPath)
			ok, err := checkDir(dir)
			if ok || err != nil {
				return dir, err
			}
		}
	}

	a = []string{fmt.Sprintf("cannot find package %q in any of:", importPath)}
	for i, v := range c.searchPaths {
		switch i {
		case 0:
			v += " (from GOROOT)"
		default:
			v += " (from GOPATH)"
		}
		a = append(a, "\t"+v)
	}

	return "", fmt.Errorf("%s", strings.Join(a, "\n"))
}

// SourceFileForPath searches loaded packages for the one containing the file
// at path.  The result is nil if the file is not considered for build by any
// of the loaded packages.
func (c *Context) SourceFileForPath(path string) *SourceFile {
	c.packagesMu.Lock()
	defer c.packagesMu.Unlock()

	for _, p := range c.packages {
		for _, f := range p.SourceFiles {
			if f.Path == path {
				return f
			}
		}
	}

	return nil
}

func (c *Context) filesForImportPath(importPath string) (dir string, sourceFiles []string, testFiles []string, err error) {
	if importPath == "C" {
		return "", nil, nil, nil
	}

	if dir, err = c.dirForImportPath(importPath); err != nil {
		return "", nil, nil, err
	}

	matches, err := filepath.Glob(filepath.Join(dir, "*.go"))
	if err != nil {
		return "", nil, nil, err
	}

	for _, match := range matches {
		b := filepath.Base(match)
		if ex := filepath.Ext(b); ex != "" {
			b = b[:len(b)-len(ex)]
		}
		isTestFile := false
		if strings.HasSuffix(b, "_test") {
			isTestFile = true
			b = b[:len(b)-len("_test")]
		}
		a := strings.Split(b, "_")
		if len(a) > 1 { // *_GOOS or *_GOARCH
			if s := a[len(a)-1]; isValidArch(s) && s != c.goarch {
				continue
			}

			if s := a[len(a)-1]; validOS[s] && s != c.goos {
				continue
			}
		}
		if len(a) > 2 { //  *_GOOS_GOARCH
			if s := a[len(a)-2]; validOS[s] && s != c.goos {
				continue
			}
		}
		switch {
		case isTestFile:
			testFiles = append(testFiles, match)
		default:
			sourceFiles = append(sourceFiles, match)
		}

	}
	return dir, sourceFiles, testFiles, nil
}

func (c *Context) load(position token.Position, importPath string, syntaxError func(*parser), errList *errorList) *Package {
	c.packagesMu.Lock()

	p := c.packages[importPath]
	if p != nil {
		c.packagesMu.Unlock()
		return p
	}

	p = newPackage(c, importPath, "", errList)
	c.packages[importPath] = p
	c.packagesMu.Unlock()

	go func() {
		defer func() {
			if err := recover(); err != nil {
				errList.Add(token.Position{}, fmt.Sprintf("%s\nPANIC: %v", debug.Stack(), err))
			}
		}()

		dir, files, _, err := c.filesForImportPath(importPath)
		if err != nil {
			close(p.ready)
			errList.Add(position, err.Error())
			return
		}

		p.Dir = dir
		p.load(position, files, syntaxError)
	}()

	return p
}

// Load finds the package in importPath and returns the resulting Package or an error if any.
func (c *Context) Load(importPath string) (*Package, error) {
	err := newErrorList(10)
	p := c.load(token.Position{}, importPath, nil, err).waitFor()
	if err := err.error(); err != nil {
		return nil, err
	}

	return p, nil
}

// SourceFile describes a source file.
type SourceFile struct {
	File          *token.File
	ImportSpecs   []*ImportSpec
	InitFunctions []Declaration
	Package       *Package
	Path          string
	Scope         *Scope // File scope.
	TopLevelDecls []Declaration
	Xref          map[token.Pos]Declaration // Enabled by DeclarationXref.
	build         bool
	f             *os.File  // Underlying src file.
	src           mmap.MMap // Valid only during parsing and checking.
	srcMu         sync.Mutex
	xref          map[Token]*Scope // Token: Resolution scope.
}

func newSourceFile(pkg *Package, path string, f *os.File, src mmap.MMap) *SourceFile {
	var (
		s     *Scope
		fset  *token.FileSet
		xref0 map[Token]*Scope
		xref  map[token.Pos]Declaration
	)
	if pkg != nil {
		s = newScope(FileScope, pkg.Scope)
		fset = pkg.ctx.FileSet
		if pkg.ctx.tweaks.declarationXref {
			xref0 = map[Token]*Scope{}
			xref = map[token.Pos]Declaration{}
		}
	} else {
		fset = token.NewFileSet()
	}
	var nm string
	if f != nil {
		nm = f.Name()
	}
	file := fset.AddFile(nm, -1, len(src))
	return &SourceFile{
		File:    file,
		Package: pkg,
		Path:    path,
		Scope:   s,
		Xref:    xref,
		build:   true,
		f:       f,
		src:     src,
		xref:    xref0,
	}
}

func (s *SourceFile) init(pkg *Package, path string) {
	s.Package = pkg
	s.ImportSpecs = s.ImportSpecs[:0]
	s.TopLevelDecls = s.TopLevelDecls[:0]
	s.Path = path
	s.build = true
	s.xref = nil
	s.Xref = nil
	if pkg != nil && pkg.ctx.tweaks.declarationXref {
		s.xref = map[Token]*Scope{}
		s.Xref = map[token.Pos]Declaration{}
	}
}

func (s *SourceFile) finit() {
	s.srcMu.Lock()
	if s.src != nil {
		s.src.Unmap()
		s.src = nil
	}
	if s.f != nil {
		s.f.Close()
		s.f = nil
	}
	s.srcMu.Unlock()
}

// Package describes a package.
type Package struct {
	Dir            string
	ImportPath     string
	ImportedBy     map[string]struct{} // R/O, key: import path.
	Imports        map[string]struct{} // R/O, key: import path.
	Name           string
	Scope          *Scope // Package scope.
	SourceFiles    []*SourceFile
	ctx            *Context
	errorList      *errorList
	fileScopeNames map[string]token.Pos
	importedByMu   sync.Mutex
	named          token.Pos
	ready          chan struct{}
}

func newPackage(ctx *Context, importPath, nm string, errorList *errorList) *Package {
	var s *Scope
	if ctx != nil {
		s = newScope(PackageScope, ctx.universe)
	}
	p := &Package{
		ImportPath:     importPath,
		ImportedBy:     map[string]struct{}{},
		Imports:        map[string]struct{}{},
		Name:           nm,
		Scope:          s,
		ctx:            ctx,
		errorList:      errorList,
		fileScopeNames: map[string]token.Pos{},
		ready:          make(chan struct{}),
	}
	if s != nil {
		s.pkg = p
	}
	return p
}

func (p *Package) load(position token.Position, paths []string, syntaxError func(*parser)) {
	defer func() {
		for _, v := range p.SourceFiles {
			v.finit()
		}
		close(p.ready)
	}()

	l := NewLexer(nil, nil)
	y := newParser(nil, nil)
	y.ignoreRedeclarations = p.ctx.tweaks.ignoreRedeclarations
	l.errHandler = y.err
	l.CommentHandler = y.commentHandler
	y.syntaxError = syntaxError

	for _, path := range paths {
		f, err := os.Open(path)
		if err != nil {
			p.errorList.Add(position, err.Error())
			return
		}

		src, err := mmap.Map(f, 0, 0)
		if err != nil {
			f.Close()
			p.errorList.Add(position, err.Error())
			return
		}

		sf := newSourceFile(p, path, f, src)
		l.init(sf.File, src)
		y.init(sf, l)
		y.file()
		switch {
		case sf.build:
			p.SourceFiles = append(p.SourceFiles, sf)
		default:
			sf.finit()
		}
	}
	//TODO p.check()
	for _, v := range p.SourceFiles {
		xref := v.Xref
		for tok, scope := range v.xref {
			if d := scope.lookup(p, v.Scope, tok); d != nil {
				xref[tok.Pos] = d
			} /*TODO- else {
				panic(fmt.Sprintf("%s: %s", v.File.Position(tok.Pos), tok.Val))
			}*/
		}
		v.xref = nil
	}
}

func (p *Package) waitFor() *Package {
	<-p.ready
	return p
}
