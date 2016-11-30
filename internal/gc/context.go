// Copyright 2016 The GC Authors. All rights reserved.
// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.

package gc

import (
	"fmt"
	"go/token"
	"os"
	"path/filepath"
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

// Context describes the context of loaded packages.
type Context struct {
	goarch        string
	goos          string
	ignoreImports bool // Test hook.
	model         model
	packages      map[string]*Package // Key: import path.
	packagesMu    sync.Mutex
	searchPaths   []string
	tags          map[string]struct{}
	universe      *Scope
}

// NewContext returns a newly created Context. tags are the build tags
// considered when loading packages having build directves (see
// https://golang.org/pkg/go/build/#hdr-Build_Constraints for details).
// searchPaths are examined when looking for a package to load.
func NewContext(goos, goarch string, tags, searchPaths []string) (*Context, error) {
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
		packages:    map[string]*Package{},
		goarch:      goarch,
		goos:        goos,
		model:       model,
		searchPaths: append([]string(nil), searchPaths...),
		tags:        tm,
		universe:    newScope(UniverseScope, nil),
	}
	return c, nil
}

/*

(1) If there is a source directory d/vendor, then, when compiling a source file
within the subtree rooted at d, import "p" is interpreted as import
"d/vendor/p" if that path names a directory containing at least one file with a
name ending in “.go”.

(2) When there are multiple possible resolutions, the most specific (longest)
path wins.

(3) The short form must always be used: no import path can contain “/vendor/”
explicitly.

(4) Import comments are ignored in vendored packages.

Update, January 2016: These rules do not apply to the “C” pseudo-package, which
is processed earlier than normal import processing. They do, however, apply to
standard library packages. If someone wants to vendor (and therefore hide the
standard library version of) “math” or even “unsafe”, they can.

Update, January 2016: The original text of the first condition (1) above read
“as import "d/vendor/p" if that exists”. It has been adjusted to require that
the path name a directory containing at least one file with a name ending in
.go, so that it is possible to vendor a/b/c without having the parent directory
vendor/a/b hide the real a/b.

----------------------------
src: golang.org/s/go15vendor

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

func (c *Context) load(pos func() token.Position, importPath string, syntaxError func(*parser), errList *errorList) *Package {
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
				errList.Add(token.Position{}, fmt.Sprint(err))
			}
		}()

		dir, files, _, err := c.filesForImportPath(importPath)
		if err != nil {
			pos := pos()
			close(p.ready)
			errList.Add(pos, err.Error())
			return
		}

		p.Dir = dir
		p.load(pos, files, syntaxError)
	}()

	return p
}

// SourceFile describes a source file.
type SourceFile struct {
	ImportSpecs   []*ImportSpec
	Package       *Package
	Path          string
	Scope         *Scope // File scope.
	TopLevelDecls []Declaration
	build         bool
	f             *os.File // Underlying src file.
	fi            *token.File
	src           mmap.MMap // Valid only during parsing and checking.
	srcMu         sync.Mutex
}

func newSourceFile(pkg *Package, path string, f *os.File, src mmap.MMap) *SourceFile {
	var s *Scope
	if pkg != nil {
		s = newScope(FileScope, pkg.Scope)
	}
	return &SourceFile{
		Package: pkg,
		Path:    path,
		Scope:   s,
		build:   true,
		f:       f,
		src:     src,
	}
}

func (s *SourceFile) init(pkg *Package, path string) {
	s.Package = pkg
	s.ImportSpecs = s.ImportSpecs[:0]
	s.TopLevelDecls = s.TopLevelDecls[:0]
	s.Path = path
	s.build = true
}

func (s *SourceFile) pos(off int) token.Position {
	s.srcMu.Lock()
	defer s.srcMu.Unlock()

	if s.src == nil {
		return token.Position{}
	}

	if s.Package.fs == nil {
		s.Package.fs = token.NewFileSet()
	}
	if s.fi == nil {
		s.fi = s.Package.fs.AddFile(s.Path, -1, len(s.src))
		s.fi.SetLinesForContent(s.src)
	}
	return s.fi.Position(s.fi.Pos(off))
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
	Dir          string
	ImportPath   string
	ImportedBy   map[string]struct{} // R/O, key: import path.
	Imports      map[string]struct{} // R/O, key: import path.
	Name         string
	Scope        *Scope // Package scope.
	SourceFiles  []*SourceFile
	ctx          *Context
	errorList    *errorList
	fs           *token.FileSet
	importedByMu sync.Mutex
	ready        chan struct{}
}

func newPackage(ctx *Context, importPath, nm string, errorList *errorList) *Package {
	var s *Scope
	if ctx != nil {
		s = newScope(PackageScope, ctx.universe)
	}
	return &Package{
		ImportPath: importPath,
		ImportedBy: map[string]struct{}{},
		Imports:    map[string]struct{}{},
		Name:       nm,
		Scope:      s,
		ctx:        ctx,
		errorList:  errorList,
		ready:      make(chan struct{}),
	}
}

func (p *Package) load(pos func() token.Position, paths []string, syntaxError func(*parser)) {
	defer func() {
		for _, v := range p.SourceFiles {
			v.finit()
		}
		p.fs = nil
		close(p.ready)
	}()

	l := newLexer(nil)
	y := newParser(nil, nil)
	l.errHandler = y.err
	l.commentHandler = y.commentHandler
	y.syntaxError = syntaxError

	for _, path := range paths {
		f, err := os.Open(path)
		if err != nil {
			p.errorList.Add(pos(), err.Error())
			return
		}

		src, err := mmap.Map(f, 0, 0)
		if err != nil {
			f.Close()
			p.errorList.Add(pos(), err.Error())
			return
		}

		sf := newSourceFile(p, path, f, src)
		l.init(src)
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
}

func (p *Package) wait() *Package {
	<-p.ready
	return p
}
