// Copyright 2016 The GC Authors. All rights reserved.
// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.

package gc

import (
	"bufio"
	"bytes"
	"flag"
	"fmt"
	"go/build"
	goparser "go/parser"
	"go/scanner"
	"go/token"
	"io/ioutil"
	"os"
	"path"
	"path/filepath"
	"regexp"
	"runtime"
	"runtime/debug"
	"sort"
	"strings"
	"sync"
	"testing"

	"github.com/cznic/lex"
	dfa "github.com/cznic/lexer"
	"github.com/cznic/mathutil"
	"github.com/cznic/sortutil"
	"github.com/cznic/y"
	"github.com/edsrzf/mmap-go"
)

func caller(s string, va ...interface{}) {
	if s == "" {
		s = strings.Repeat("%v ", len(va))
	}
	_, fn, fl, _ := runtime.Caller(2)
	fmt.Fprintf(os.Stderr, "// caller: %s:%d: ", path.Base(fn), fl)
	fmt.Fprintf(os.Stderr, s, va...)
	fmt.Fprintln(os.Stderr)
	_, fn, fl, _ = runtime.Caller(1)
	fmt.Fprintf(os.Stderr, "// \tcallee: %s:%d: ", path.Base(fn), fl)
	fmt.Fprintln(os.Stderr)
	os.Stderr.Sync()
}

var dbgMu sync.Mutex

func dbg(s string, va ...interface{}) {
	dbgMu.Lock()
	defer dbgMu.Unlock()

	if s == "" {
		s = strings.Repeat("%v ", len(va))
	}
	_, fn, fl, _ := runtime.Caller(1)
	fmt.Fprintf(os.Stderr, "// dbg %s:%d: ", path.Base(fn), fl)
	fmt.Fprintf(os.Stderr, s, va...)
	fmt.Fprintln(os.Stderr)
	os.Stderr.Sync()
}

func TODO(...interface{}) string { //TODOOK
	_, fn, fl, _ := runtime.Caller(1)
	return fmt.Sprintf("// TODO: %s:%d:\n", path.Base(fn), fl) //TODOOK
}

func stack() []byte { return debug.Stack() }

func use(...interface{}) {}

func init() {
	use(caller, dbg, TODO, (*parser).todo, stack) //TODOOK
	_, file, _, ok := runtime.Caller(0)
	if !ok {
		panic("internal error")
	}

	gopaths := filepath.SplitList(os.Getenv("GOPATH"))
	for _, v := range gopaths {
		gp := filepath.Join(v, "src")
		path, err := filepath.Rel(gp, file)
		if err != nil {
			continue
		}

		selfImportPath = filepath.Dir(path)
		return
	}

	panic("internal error")
}

// ============================================================================

const (
	lexFile   = "testdata/scanner/scanner.l"
	yaccCover = "testdata/parser/ycover.go"
	yaccFile  = "testdata/parser/parser.y"
)

type yParser struct {
	*y.Parser
	reports   [][]byte
	terminals []*y.Symbol
	tok2sym   map[token.Token]*y.Symbol
}

var (
	_   = flag.Bool("closures", false, "closures")        //TODOOK
	_   = flag.String("out", "", "where to put y.output") //TODOOK
	oN  = flag.Int("N", -1, "")
	oRE = flag.String("re", "", "regexp")

	selfImportPath string

	errCheckDisbled     = []byte("////")
	errCheckMark1       = []byte("// ERROR ")
	errCheckMark2       = []byte("// GC_ERROR ")
	errCheckPatterns    = regexp.MustCompile(`"([^"]*)"`)
	generalCommentEnd   = []byte("*/")
	generalCommentStart = []byte("/*")

	lexL = func() *lex.L {
		lf, err := os.Open(lexFile)
		if err != nil {
			panic(err)
		}

		defer lf.Close()

		l, err := lex.NewL(lexFile, bufio.NewReader(lf), false, false)
		if err != nil {
			panic(err)
		}

		return l
	}()

	yp0 = func() *yParser {
		var closures bool
		var fn string
		for i, v := range os.Args {
			if i == 0 {
				continue
			}

			switch v {
			case "-closures":
				closures = true
			case "-out":
				fn = os.Args[i+1]
			}
		}
		fset := token.NewFileSet()
		var out bytes.Buffer
		p, err := y.ProcessFile(fset, yaccFile, &y.Options{
			Closures:  closures,
			Reducible: true,
			Report:    &out,
		})
		if fn != "" {
			if err := ioutil.WriteFile(fn, out.Bytes(), 0644); err != nil {
				panic(err)
			}
		}

		if err != nil {
			panic(err)
		}

		reports := make([][]byte, len(p.States))
		rep := out.Bytes()
		sep := []byte("\ns") // "\nstate "
		s := 0
		for i := range reports {
			e := bytes.Index(rep[s:], sep)
			if e < 0 {
				e = len(rep[s:]) - 1
			}
			reports[i] = rep[s : s+e]
			s = s + e + 1
		}

		m := make(map[token.Token]*y.Symbol, len(p.Syms))
		for k, v := range p.Syms {
			if !v.IsTerminal || k == "BODY" || k[0] == '_' {
				continue
			}

			switch {
			case k[0] >= 'A' && k[0] <= 'Z':
				if tok, ok := str2token[k]; ok {
					m[tok] = v
					break
				}

				l := v.LiteralString
				if l == "" {
					panic(fmt.Errorf("no token for %q", k))
				}

				if tok, ok := str2token[l[1:len(l)-1]]; ok {
					m[tok] = v
					break
				}

				panic(k)
			case k[0] == '\'':
				tok := str2token[k[1:2]]
				m[tok] = v
			default:
			}
		}
		m[token.EOF] = p.Syms["$end"]
		m[tokenBODY] = p.Syms["BODY"]
		var t []*y.Symbol
		for _, v := range p.Syms {
			if v.IsTerminal {
				t = append(t, v)
			}
		}
		return &yParser{
			Parser:    p,
			reports:   reports,
			terminals: t,
			tok2sym:   m,
		}
	}()

	str2token = func() map[string]token.Token {
		m := map[string]token.Token{}
		for i := token.IDENT; i <= maxTokenToken; i++ {
			s := strings.ToUpper(i.String())
			if _, ok := m[s]; ok {
				panic(fmt.Errorf("internal error %q", s))
			}

			m[s] = i
		}
		m["ILLEGAL"] = token.ILLEGAL
		m["«"] = tokenLTLT
		m["»"] = tokenGTGT
		return m
	}()

	gorootPackages []*Package

	gorootFiles = func() (r []string) {
		if err := filepath.Walk(filepath.Join(runtime.GOROOT(), "src"), func(path string, info os.FileInfo, err error) error {
			if err != nil {
				return err
			}

			if !info.IsDir() {
				return nil
			}

			if base := filepath.Base(path); strings.HasPrefix(base, ".") ||
				strings.HasPrefix(base, "_") ||
				base == "testdata" {
				return filepath.SkipDir
			}

			p, err := build.ImportDir(path, 0)
			if err != nil {
				if _, ok := err.(*build.NoGoError); ok {
					return nil
				}

				return err
			}

			ctx := &Context{fset: token.NewFileSet()}
			pkg := newPackage(ctx, p.ImportPath, p.Name, nil)
			for _, v := range p.GoFiles {
				path := filepath.Join(p.Dir, v)
				r = append(r, path)
				pkg.SourceFiles = append(pkg.SourceFiles, newSourceFile(pkg, path, nil, nil))
			}
			gorootPackages = append(gorootPackages, pkg)
			for _, v := range p.TestGoFiles {
				r = append(r, filepath.Join(p.Dir, v))
			}
			return nil
		}); err != nil {
			panic(err)
		}

		gorootPackages = gorootPackages[:len(gorootPackages):len(gorootPackages)]
		return r[:len(r):len(r)]
	}()

	errchkFiles = func() (r []string) {
		if err := filepath.Walk(filepath.Join(runtime.GOROOT(), "test"), func(path string, info os.FileInfo, err error) error {
			if err != nil {
				return err
			}

			if info.IsDir() {
				return nil
			}

			if base := filepath.Base(path); strings.HasPrefix(base, ".") ||
				!strings.HasSuffix(base, ".go") {
				return nil
			}

			r = append(r, path)
			return nil
		}); err != nil {
			panic(err)
		}

		return r[:len(r):len(r)]
	}()

	stdLibPackages []*Package

	stdLibFiles = func() (r []string) {
		cmd := filepath.Join(runtime.GOROOT(), "src", "cmd")
		for _, v := range gorootFiles {
			if !strings.Contains(v, cmd) && !strings.HasSuffix(v, "_test.go") {
				r = append(r, v)
			}
		}
		for _, v := range gorootPackages {
			if !strings.HasPrefix(v.SourceFiles[0].Path, cmd) {
				stdLibPackages = append(stdLibPackages, v)
			}
		}
		stdLibPackages = stdLibPackages[:len(stdLibPackages):len(stdLibPackages)]
		return r[:len(r):len(r)]
	}()
)

func errString(err error) string {
	var b bytes.Buffer
	scanner.PrintError(&b, err)
	return strings.TrimSpace(b.String())
}

func testScannerStates(t *testing.T) {
	mn := len(lexL.Dfa)
	mn0 := mn
	m := make([]bool, mn+1) // 1-based state.Index.
	fset := token.NewFileSet()
	var ss scanner.Scanner
	l := NewLexer(nil, nil)
	nerr := 0

	var cases, sum int
	var f func(string, *dfa.NfaState)
	f = func(prefix string, s *dfa.NfaState) {
		if nerr >= 10 {
			return
		}

		if m[s.Index] {
			return
		}

		m[s.Index] = true

		if len(s.NonConsuming) != 0 {
			panic("internal error")
		}

		next := make([]*dfa.NfaState, classNext)
		for _, e := range s.Consuming {
			switch x := e.(type) {
			case *dfa.RangesEdge:
				if x.Invert {
					panic("internal error")
				}

				for _, v := range x.Ranges.R16 {
					for c := v.Lo; c <= v.Hi; c += v.Stride {
						if c >= classNext {
							continue
						}

						if next[c] != nil {
							panic("internal error")
						}

						next[c] = x.Targ
					}
				}
				for _, v := range x.Ranges.R32 {
					for c := v.Lo; c <= v.Hi; c += v.Stride {
						if c >= classNext {
							continue
						}

						if next[c] != nil {
							panic("internal error")
						}

						next[c] = x.Targ
					}
				}
			case *dfa.RuneEdge:
				c := x.Rune
				if c >= classNext {
					continue
				}

				if next[c] != nil {
					panic("internal error")
				}

				next[c] = x.Targ
			default:
				panic(fmt.Errorf("internal error: %T", x))
			}
		}
		for c, nx := range next {
			iCase := cases
			cases++
			src := prefix
			switch c {
			case classEOF:
				// nop
			case classNonASCII:
				src += "á"
			case classLTLT, classGTGT, classBOM:
				src += "@"
			default:
				src += string(c)
			}

			fi := fset.AddFile("", -1, len(src))
			fi2 := fset.AddFile("", -1, len(src))
			errCnt := 0
			b := []byte(src)
			var errs, errs2 scanner.ErrorList
			l.init(fi, b)
			l.errHandler = func(position token.Position, msg string, args ...interface{}) {
				errCnt++
				errs.Add(position, fmt.Sprintf(msg, args...))
			}
			ss.Init(fi2, b, func(pos token.Position, msg string) {
				errs2.Add(pos, msg)
			}, 0)
			sum += len(src)
			for i := 0; nerr <= 10; i++ {
				errs = nil
				errs2 = nil
				l.errorCount = 0
				ss.ErrorCount = 0
				off, tok := l.Scan()
				pos := l.position(off)
				lit := string(l.lit)
				p2, tok2, lit2 := ss.Scan()
				pos2 := fi2.Position(p2)
				if n := *oN; n < 0 || n == iCase {
					if g, e := l.errorCount != 0, ss.ErrorCount != 0; g != e {
						nerr++
						t.Errorf(
							"%6d: errors[%d] %q(|% x|), %v %v\nall got\n%s\nall exp\n%s",
							iCase, i, src, src, l.errorCount, ss.ErrorCount, errString(errs), errString(errs2),
						)
					}
					if g, e := pos.Filename, pos2.Filename; g != e {
						nerr++
						t.Errorf("%6d: pos.Filename[%d] %q(|% x|), %v %v (%v %v)", iCase, i, src, src, g, e, pos, pos2)
					}
					if g, e := int(pos.Line), pos2.Line; g != e {
						nerr++
						t.Errorf("%6d: pos.Line[%d] %q(|% x|), %v %v (%v %v)", iCase, i, src, src, g, e, pos, pos2)
					}
					if g, e := int(pos.Column), pos2.Column; g != e {
						nerr++
						t.Errorf("%6d: pos.Column[%d] %q(|% x|), %v %v (%v %v)", iCase, i, src, src, g, e, pos, pos2)
					}
					if g, e := int(pos.Offset), pos2.Offset; g != e {
						nerr++
						t.Errorf("%6d: pos.Offset[%d] %q(|% x|), %v %v (%v %v)", iCase, i, src, src, g, e, pos, pos2)
					}
					if g, e := tok, tok2; g != e {
						// Whitelist cases like "f..3", go/scanner differs from the compiler lexer.
						if tok == token.PERIOD && tok2 == token.ILLEGAL && lit == "." && strings.Contains(src, "..") {
							break
						}

						nerr++
						t.Errorf("%6d: tok[%d] %q(|% x|) %s %s", iCase, i, src, src, g, e)

					}
					if l.errorCount+ss.ErrorCount != 0 || tok == token.ILLEGAL {
						continue
					}

					if lit2 == "" && tok2 != token.EOF {
						lit2 = tok2.String()
					}
					if g, e := lit, lit2; g != e {
						nerr++
						t.Errorf("%6d: lit[%d] %q(|% x|), %q(|% x|) %q(|% x|)", iCase, i, src, src, g, g, e, e)
					}
					if nerr >= 10 {
						return
					}
				}
				if tok == token.EOF || tok2 == token.EOF {
					break
				}
			}
			if c == classEOF || nx == nil {
				continue
			}

			f(src, nx)
		}
		mn--
	}
	f("", lexL.Dfa[0])
	if mn != 0 {
		t.Errorf("states covered: %d/%d", mn0-mn, mn0)
	} else {
		t.Logf("states covered: %d/%d", mn0-mn, mn0)
	}
	t.Logf("test cases %v, total src len %v", cases, sum)
}

func testScannerBugs(t *testing.T) {
	type toks []struct {
		off int
		pos string
		tok token.Token
		lit string
	}
	fset := token.NewFileSet()
	l := NewLexer(nil, nil)
	n := *oN
	nerr := 0
	cases := 0
	for i, v := range []struct {
		src  string
		toks toks
	}{
		{" (", toks{{1, "1:2", token.LPAREN, "("}}},
		{" (\n", toks{{1, "1:2", token.LPAREN, "("}}},
		{" z ", toks{{1, "1:2", token.IDENT, "z"}, {3, "1:4", token.SEMICOLON, "\n"}}},
		{" z", toks{{1, "1:2", token.IDENT, "z"}, {2, "1:3", token.SEMICOLON, "\n"}}},
		{" za ", toks{{1, "1:2", token.IDENT, "za"}, {4, "1:5", token.SEMICOLON, "\n"}}},
		{" za", toks{{1, "1:2", token.IDENT, "za"}, {3, "1:4", token.SEMICOLON, "\n"}}},
		{" « ", toks{{1, "1:2", tokenLTLT, "«"}}},
		{" » ", toks{{1, "1:2", tokenGTGT, "»"}, {4, "1:5", token.SEMICOLON, "\n"}}},
		{"", nil},
		{"'\\U00000000'!", toks{{0, "1:1", token.CHAR, "'\\U00000000'"}, {12, "1:13", token.NOT, "!"}}},
		{"'\\U00000000'", toks{{0, "1:1", token.CHAR, "'\\U00000000'"}, {12, "1:13", token.SEMICOLON, "\n"}}},
		{"'\\u0000'!", toks{{0, "1:1", token.CHAR, "'\\u0000'"}, {8, "1:9", token.NOT, "!"}}},
		{"'\\u0000'", toks{{0, "1:1", token.CHAR, "'\\u0000'"}, {8, "1:9", token.SEMICOLON, "\n"}}},
		{"'\\x00'!", toks{{0, "1:1", token.CHAR, "'\\x00'"}, {6, "1:7", token.NOT, "!"}}},
		{"'\\x00'", toks{{0, "1:1", token.CHAR, "'\\x00'"}, {6, "1:7", token.SEMICOLON, "\n"}}},
		{"( ", toks{{0, "1:1", token.LPAREN, "("}}},
		{"(", toks{{0, "1:1", token.LPAREN, "("}}},
		{"/***/func", toks{{5, "1:6", token.FUNC, "func"}}},
		{"/**/func", toks{{4, "1:5", token.FUNC, "func"}}},
		{"/*\n */\nfunc ", toks{{7, "3:1", token.FUNC, "func"}}},
		{"/*\n *\n */\nfunc ", toks{{10, "4:1", token.FUNC, "func"}}},
		{"/*\n*/\nfunc ", toks{{6, "3:1", token.FUNC, "func"}}},
		{"/*\n\n*/\nfunc ", toks{{7, "4:1", token.FUNC, "func"}}},
		{"//", nil},
		{"//\n", nil},
		{"//\n//", nil},
		{"//\n//\n", nil},
		{"//\n//\n@", toks{{6, "3:1", token.ILLEGAL, "@"}}},
		{"//\n//\nz", toks{{6, "3:1", token.IDENT, "z"}, {7, "3:2", token.SEMICOLON, "\n"}}},
		{"//\n//\nz1", toks{{6, "3:1", token.IDENT, "z1"}, {8, "3:3", token.SEMICOLON, "\n"}}},
		{"//\n@", toks{{3, "2:1", token.ILLEGAL, "@"}}},
		{"//\nz", toks{{3, "2:1", token.IDENT, "z"}, {4, "2:2", token.SEMICOLON, "\n"}}},
		{"//\nz1", toks{{3, "2:1", token.IDENT, "z1"}, {5, "2:3", token.SEMICOLON, "\n"}}},
		{";\xf0;", toks{{0, "1:1", token.SEMICOLON, ";"}, {1, "1:2", token.IDENT, "\xf0"}, {2, "1:3", token.SEMICOLON, ";"}}},
		{"\"\\U00000000\"!", toks{{0, "1:1", token.STRING, "\"\\U00000000\""}, {12, "1:13", token.NOT, "!"}}},
		{"\"\\U00000000\"", toks{{0, "1:1", token.STRING, "\"\\U00000000\""}, {12, "1:13", token.SEMICOLON, "\n"}}},
		{"\"\\u0000\"!", toks{{0, "1:1", token.STRING, "\"\\u0000\""}, {8, "1:9", token.NOT, "!"}}},
		{"\"\\u0000\"", toks{{0, "1:1", token.STRING, "\"\\u0000\""}, {8, "1:9", token.SEMICOLON, "\n"}}},
		{"\"\\x00\"!", toks{{0, "1:1", token.STRING, "\"\\x00\""}, {6, "1:7", token.NOT, "!"}}},
		{"\"\\x00\"", toks{{0, "1:1", token.STRING, "\"\\x00\""}, {6, "1:7", token.SEMICOLON, "\n"}}},
		{"\xf0", toks{{0, "1:1", token.IDENT, "\xf0"}, {1, "1:2", token.SEMICOLON, "\n"}}},
		{"\xf0;", toks{{0, "1:1", token.IDENT, "\xf0"}, {1, "1:2", token.SEMICOLON, ";"}}},
		{"a @= b", toks{{0, "1:1", token.IDENT, "a"}, {2, "1:3", token.ILLEGAL, "@"}, {3, "1:4", token.ASSIGN, "="}, {5, "1:6", token.IDENT, "b"}, {6, "1:7", token.SEMICOLON, "\n"}}},
		{"a/**/", toks{{0, "1:1", token.IDENT, "a"}, {1, "1:2", token.SEMICOLON, "\n"}}},
		{"a/**//**/", toks{{0, "1:1", token.IDENT, "a"}, {1, "1:2", token.SEMICOLON, "\n"}}},
		{"a/*\n*/", toks{{0, "1:1", token.IDENT, "a"}, {1, "1:2", token.SEMICOLON, "\n"}}},
		{"a/*\n*//**/", toks{{0, "1:1", token.IDENT, "a"}, {1, "1:2", token.SEMICOLON, "\n"}}},
		{"a//", toks{{0, "1:1", token.IDENT, "a"}, {1, "1:2", token.SEMICOLON, "\n"}}},
		{"a//\n", toks{{0, "1:1", token.IDENT, "a"}, {1, "1:2", token.SEMICOLON, "\n"}}},
		{"a«z", toks{{0, "1:1", token.IDENT, "a"}, {1, "1:2", tokenLTLT, "«"}, {3, "1:4", token.IDENT, "z"}, {4, "1:5", token.SEMICOLON, "\n"}}},
		{"a»z", toks{{0, "1:1", token.IDENT, "a"}, {1, "1:2", tokenGTGT, "»"}, {3, "1:4", token.IDENT, "z"}, {4, "1:5", token.SEMICOLON, "\n"}}},
		{"d/*\\\n*/0", toks{{0, "1:1", token.IDENT, "d"}, {1, "1:2", token.SEMICOLON, "\n"}, {7, "2:3", token.INT, "0"}, {8, "2:4", token.SEMICOLON, "\n"}}},
		{"import ( ", toks{{0, "1:1", token.IMPORT, "import"}, {7, "1:8", token.LPAREN, "("}}},
		{"import (", toks{{0, "1:1", token.IMPORT, "import"}, {7, "1:8", token.LPAREN, "("}}},
		{"import (\n", toks{{0, "1:1", token.IMPORT, "import"}, {7, "1:8", token.LPAREN, "("}}},
		{"import (\n\t", toks{{0, "1:1", token.IMPORT, "import"}, {7, "1:8", token.LPAREN, "("}}},
		{"z ", toks{{0, "1:1", token.IDENT, "z"}, {2, "1:3", token.SEMICOLON, "\n"}}},
		{"z w", toks{{0, "1:1", token.IDENT, "z"}, {2, "1:3", token.IDENT, "w"}, {3, "1:4", token.SEMICOLON, "\n"}}},
		{"z", toks{{0, "1:1", token.IDENT, "z"}, {1, "1:2", token.SEMICOLON, "\n"}}},
		{"za ", toks{{0, "1:1", token.IDENT, "za"}, {3, "1:4", token.SEMICOLON, "\n"}}},
		{"za wa", toks{{0, "1:1", token.IDENT, "za"}, {3, "1:4", token.IDENT, "wa"}, {5, "1:6", token.SEMICOLON, "\n"}}},
		{"za", toks{{0, "1:1", token.IDENT, "za"}, {2, "1:3", token.SEMICOLON, "\n"}}},
		{"«", toks{{0, "1:1", tokenLTLT, "«"}}},
		{"«a", toks{{0, "1:1", tokenLTLT, "«"}, {2, "1:3", token.IDENT, "a"}, {3, "1:4", token.SEMICOLON, "\n"}}},
		{"««", toks{{0, "1:1", tokenLTLT, "«"}, {2, "1:3", tokenLTLT, "«"}}},
		{"«»", toks{{0, "1:1", tokenLTLT, "«"}, {2, "1:3", tokenGTGT, "»"}, {4, "1:5", token.SEMICOLON, "\n"}}},
		{"»", toks{{0, "1:1", tokenGTGT, "»"}, {2, "1:3", token.SEMICOLON, "\n"}}},
		{"»a", toks{{0, "1:1", tokenGTGT, "»"}, {2, "1:3", token.IDENT, "a"}, {3, "1:4", token.SEMICOLON, "\n"}}},
		{"»«", toks{{0, "1:1", tokenGTGT, "»"}, {2, "1:3", tokenLTLT, "«"}}},
		{"»»", toks{{0, "1:1", tokenGTGT, "»"}, {2, "1:3", tokenGTGT, "»"}, {4, "1:5", token.SEMICOLON, "\n"}}},
	} {
		if n >= 0 && i != n {
			continue
		}

		cases++
		src := v.src
		bsrc := []byte(src)
		fi := fset.AddFile("", -1, len(src))
		l.init(fi, bsrc)
		for j, v := range v.toks {
			off, tok := l.Scan()
			if g, e := int(off), v.off; g != e {
				nerr++
				t.Errorf("%v off[%d] %q(|% x|) %v %v", i, j, src, src, g, e)
			}
			if g, e := l.position(off).String(), v.pos; g != e {
				nerr++
				t.Errorf("%v pos[%d] %q(|% x|) %q %q", i, j, src, src, g, e)
			}
			if g, e := tok, v.tok; g != e {
				nerr++
				t.Errorf("%v tok[%d] %q(|% x|) %q %q", i, j, src, src, g, e)
			}
			if g, e := string(l.lit), v.lit; g != e {
				nerr++
				t.Errorf("%v lit[%d] %q(|% x|) %q(|% x|) %q(|% x|)", i, j, src, src, g, g, e, e)
			}
			if nerr >= 10 {
				return
			}
		}
		off, tok := l.Scan()
		if g, e := tok, token.EOF; g != e {
			nerr++
			t.Errorf("%v tok %q(|% x|) %q %q", i, src, src, g, e)
		}
		if g, e := int(off), len(src); g != e {
			nerr++
			t.Errorf("%v off %q(|% x|) %v %v", i, src, src, g, e)
		}
		if nerr >= 10 {
			return
		}
	}
	t.Logf("test cases: %v", cases)
}

func testScanner(t *testing.T, paths []string) {
	fset := token.NewFileSet()
	var s scanner.Scanner
	l := NewLexer(nil, nil)
	sum := 0
	toks := 0
	files := 0
	grt := filepath.Join(runtime.GOROOT(), "test")
outer:
	for _, path := range paths {
		src, err := ioutil.ReadFile(path)
		if err != nil {
			t.Fatal(err)
		}

		sum += len(src)
		fi := fset.AddFile(path, -1, len(src))
		fi2 := fset.AddFile(path, -1, len(src))
		var se scanner.ErrorList
		l.init(fi, src)
		l.fname = &path
		l.CommentHandler = func(off int32, lit []byte) {
			if bytes.HasPrefix(lit, lineDirective) {
				lit = bytes.TrimSpace(lit[len(lineDirective):])
				if i := bytes.IndexByte(lit, ':'); i > 0 && i < len(lit)-1 { //TODO last index.
					fn := lit[:i]
					ln := 0
					for _, c := range lit[i+1:] {
						if c >= '0' && c <= '9' {
							ln = 10*ln + int(c-'0')
							continue
						}

						return
					}
					s := filepath.Clean(string(fn))
					if !filepath.IsAbs(s) {
						s = filepath.Join(filepath.Dir(path), s)
					}
					if l.off != int32(len(l.src)) {
						l.fname = &s
						l.file.AddLineInfo(int(l.off)-1, s, ln-1)
					}
				}
			}
		}
		l.errHandler = func(position token.Position, msg string, arg ...interface{}) {
			se.Add(position, fmt.Sprintf(msg, arg...))
		}
		s.Init(fi2, src, nil, 0)
		files++
		for {
			l.errorCount = 0
			s.ErrorCount = 0

			off, gt := l.Scan()
			if gt == tokenBOM {
				gt = token.ILLEGAL
			}
			toks++
			glit := string(l.lit)
			pos, et, lit := s.Scan()
			position := fi2.Position(pos)
			g := l.position(off)
			if e := position; g != e {
				t.Errorf("%s: position mismatch, expected %s", g, e)
				continue outer
			}

			if l.errorCount != 0 && s.ErrorCount == 0 {
				t.Error(errString(se))
				continue outer
			}

			if gt == token.EOF {
				if et != token.EOF {
					t.Errorf("%s: unexpected eof", position)
					continue outer
				}

				break
			}

			if g, e := gt, et; g != e {
				// Whitelist $GOROOT/test/fixedbugs/issue11359.go:11:5: token mismatch "IDENT" "ILLEGAL"
				if gt == token.IDENT && et == token.ILLEGAL && strings.HasPrefix(path, grt) {
					continue outer
				}

				// Whitelist $GOROOT/test/syntax/ddd.go:10:5: token mismatch "." "ILLEGAL"
				if gt == token.PERIOD && et == token.ILLEGAL && strings.HasPrefix(path, grt) {
					continue outer
				}

				// Whitelist testdata/errchk/issue15292/0.go:33:12: token mismatch "token(87)" "ILLEGAL"
				if (gt == tokenLTLT || gt == tokenGTGT) && et == token.ILLEGAL && strings.HasPrefix(path, "testdata") {
					continue outer
				}

				t.Errorf("%s: token mismatch %q %q", position, g, e)
				continue outer
			}

			if lit == "" {
				lit = gt.String()
			}
			if g, e := glit, lit; g != e {
				// Whitelist $GOROOT/test/fixedbugs/bug163.go:10:2: literal mismatch "x⊛y" "x"
				if gt == token.IDENT && strings.HasPrefix(glit, lit) && strings.HasPrefix(path, grt) {
					continue outer
				}

				t.Errorf("%s: literal mismatch %q %q", position, g, e)
				continue outer
			}
		}
	}
	t.Logf("files: %v, toks: %v, bytes %v", files, toks, sum)
}

func TestScanner(t *testing.T) {
	_ = t.Run("States", testScannerStates) && //TODOOK
		t.Run("Bugs", testScannerBugs) &&
		t.Run("GOROOT", func(t *testing.T) { testScanner(t, gorootFiles) }) &&
		t.Run("Errchk", func(t *testing.T) { testScanner(t, errchkFiles) })
}

func BenchmarkScanner(b *testing.B) {
	b.Run("StdGo", func(b *testing.B) {
		c := make(chan error, len(stdLibFiles))
		fset := token.NewFileSet()
		b.ResetTimer()
		var sum int
		for i := 0; i < b.N; i++ {
			sum = 0
			for _, v := range stdLibFiles {
				go func(v string) {
					src, err := ioutil.ReadFile(v)
					if err != nil {
						c <- err
						return
					}

					sum += len(src)
					var s scanner.Scanner
					s.Init(fset.AddFile(v, -1, len(src)), src, nil, 0)
					for {
						if _, tok, _ := s.Scan(); tok == token.EOF {
							break
						}
					}
					c <- nil
				}(v)
			}
			for range stdLibFiles {
				if err := <-c; err != nil {
					b.Fatal(err)
				}
			}
		}
		b.SetBytes(int64(sum))
	})

	b.Run("Std", func(b *testing.B) {
		c := make(chan error, len(stdLibFiles))
		fset := token.NewFileSet()
		b.ResetTimer()
		var sum int
		for i := 0; i < b.N; i++ {
			sum = 0
			for _, v := range stdLibFiles {
				go func(v string) {
					f0, err := os.Open(v)
					if err != nil {
						c <- err
						return
					}

					defer f0.Close()

					src, err := mmap.Map(f0, 0, 0)
					if err != nil {
						c <- err
						return
					}

					defer src.Unmap()

					sum += len(src)
					l := NewLexer(fset.AddFile(v, -1, len(src)), src)
					for {
						if _, tok := l.Scan(); tok == token.EOF {
							break
						}
					}
					c <- nil
				}(v)
			}
			for range stdLibFiles {
				if err := <-c; err != nil {
					b.Fatal(err)
				}
			}
		}
		b.SetBytes(int64(sum))
	})
}

func BenchmarkParser(b *testing.B) {
	var sum int
	b.Run("StdGo", func(b *testing.B) {
		c := make(chan error, len(stdLibFiles))
		fset := token.NewFileSet()
		b.ResetTimer()
		for i := 0; i < b.N; i++ {
			sum = 0
			for _, v := range stdLibFiles {
				go func(v string) {
					src, err := ioutil.ReadFile(v)
					if err != nil {
						c <- err
						return
					}

					sum += len(src)
					_, err = goparser.ParseFile(fset, v, src, 0)
					c <- err
				}(v)
			}
			for range stdLibFiles {
				if err := <-c; err != nil {
					b.Fatal(err)
				}
			}
		}
		b.SetBytes(int64(sum))
	})

	b.Run("Std", func(b *testing.B) {
		a := make([]*Package, len(stdLibPackages))
		errorList := newErrorList(1)
		b.ResetTimer()
		for i := 0; i < b.N; i++ {
			b.StopTimer()
			ctx, err := newTestContext()
			if err != nil {
				b.Fatal(err)
			}

			b.StartTimer()
			for i, v := range stdLibPackages {
				a[i] = ctx.load(token.Position{}, v.ImportPath, nil, errorList)
			}
			for _, v := range a {
				v.waitFor()
			}
			if err := errorList.error(); err != nil {
				b.Fatal(err)
			}
		}
		if sum != 0 {
			b.SetBytes(int64(sum))
		}
	})
}

type ylex struct {
	*Lexer
	lbrace        int
	lbraceRule    int
	lbraceStack   []int
	loophack      bool
	loophackStack []bool
	p             *yparser
	pos           token.Position
	tok           token.Token
}

func (l *ylex) init(file *token.File, src []byte) {
	l.Lexer.init(file, src)
	l.lbrace = 0
	l.lbraceStack = l.lbraceStack[:0]
	l.loophack = false
	l.loophackStack = l.loophackStack[:0]
}

func newYlex(l *Lexer, p *yparser) *ylex {
	yl := &ylex{Lexer: l, p: p}
	for _, v := range p.Rules {
		if v.Sym.Name == "lbrace" {
			yl.lbraceRule = v.RuleNum
			break
		}
	}
	return yl
}

func (l *ylex) lex() (token.Position, *y.Symbol) {
	off, tok := l.Scan()
	l.pos = l.position(off)
	sym, ok := l.p.tok2sym[tok]
	if !ok {
		panic(fmt.Sprintf("%s: missing symbol for token %q", l.pos, tok))
	}

	switch tok {
	case token.FOR, token.IF, token.SELECT, token.SWITCH:
		l.loophack = true
	case token.LPAREN, token.LBRACK:
		if l.loophack || len(l.loophackStack) != 0 {
			l.loophackStack = append(l.loophackStack, l.loophack)
			l.loophack = false
		}
	case token.RPAREN, token.RBRACK:
		if n := len(l.loophackStack); n != 0 {
			l.loophack = l.loophackStack[n-1]
			l.loophackStack = l.loophackStack[:n-1]
		}
	case token.LBRACE:
		l.lbrace++
		if l.loophack {
			tok = tokenBODY
			sym = l.p.tok2sym[tok]
			l.loophack = false
		}
	case token.RBRACE:
		l.lbrace--
		if n := len(l.lbraceStack); n != 0 && l.lbraceStack[n-1] == l.lbrace {
			l.lbraceStack = l.lbraceStack[:n-1]
			l.loophack = true
		}
	}
	l.tok = tok
	return l.pos, sym
}

func (l *ylex) fixLbr() {
	n := l.lbrace - 1
	switch l.tok {
	case token.RBRACE:
		l.loophack = true
		return
	case token.LBRACE:
		n--
	}

	l.lbraceStack = append(l.lbraceStack, n)
}

type yparser struct {
	*yParser
	reduce func(int)
	trace  func(int)
	yyS    []int
	yySyms []*y.Symbol
	yychar *y.Symbol
}

func newYParser(reduce, trace func(int)) *yparser {
	return &yparser{
		yParser: yp0,
		reduce:  reduce,
		trace:   trace,
	}
}

func (p *yparser) parse(lex func(int) *y.Symbol) error {
	yystate := 0
	p.yyS = p.yyS[:0]
	p.yySyms = p.yySyms[:0]
	p.yychar = nil
	for {
		p.yyS = append(p.yyS, yystate)
		if p.trace != nil {
			p.trace(yystate)
		}
		if p.yychar == nil {
			p.yychar = lex(yystate)
		}
		switch typ, arg := p.action(yystate, p.yychar).Kind(); typ {
		case 'a':
			return nil
		case 's':
			p.yySyms = append(p.yySyms, p.yychar)
			p.yychar = nil
			yystate = arg
		case 'r':
			rule := p.Rules[arg]
			if p.reduce != nil {
				p.reduce(rule.RuleNum)
			}
			n := len(p.yyS)
			m := len(rule.Components)
			p.yyS = p.yyS[:n-m]
			p.yySyms = append(p.yySyms[:n-m-1], rule.Sym)
			n -= m
			if p.trace != nil {
				p.trace(p.yyS[n-1])
			}
			_, yystate = p.action(p.yyS[n-1], rule.Sym).Kind()
		default:
			return p.fail(yystate)
		}
	}
}

func (p *yparser) fail(yystate int) error {
	var a []string
	for _, v := range p.Table[yystate] {
		nm := v.Sym.Name
		if nm == "$end" {
			nm = "EOF"
		}
		if l := v.Sym.LiteralString; l != "" {
			nm = l
		}
		a = append(a, nm)
	}
	sort.Strings(a)
	return fmt.Errorf("no action for %s in state %d, follow set: [%v]", p.yychar, yystate, strings.Join(a, ", "))
}

func (p *yparser) report(states []int) string {
	var b bytes.Buffer
	for _, state := range states {
		b.Write(p.reports[state])
		b.WriteString("----\n")
	}
	return b.String()
}

func (p *yparser) sym2str(sym *y.Symbol) string {
	nm := sym.Name
	if nm == "$end" {
		return ""
	}

	if nm[0] == '\'' {
		return nm[1:2]
	}

	if nm[0] >= 'A' && nm[0] <= 'Z' {
		if s := sym.LiteralString; s != "" {
			return s[1 : len(s)-1]
		}

		if s := p.tok2str(str2token[nm]); s != "" {
			return s
		}
	}

	return "@"
}

func (*yparser) tok2str(tok token.Token) string {
	switch tok {
	case token.ILLEGAL:
		return "@"
	case token.COMMENT, token.EOF, tokenNL, tokenLTLT, tokenGTGT:
		return ""
	case token.IDENT:
		return "a"
	case token.INT:
		return "1"
	case token.FLOAT:
		return "2.3"
	case token.IMAG:
		return "4i"
	case token.CHAR:
		return "'b'"
	case token.STRING:
		return `"c"`
	case tokenBODY:
		return "{"
	default:
		return tok.String()
	}
}

func (p *yparser) newCover() map[int]struct{} {
	r := make(map[int]struct{}, len(p.States))
	for i := range p.States {
		r[i] = struct{}{}
	}
	return r
}

func (p *yparser) followList(state int) (r []*y.Symbol) {
	for _, v := range p.Table[state] {
		if v.Sym.IsTerminal {
			r = append(r, v.Sym)
		}
	}
	return r
}

func (p *yparser) followSet(state int) map[*y.Symbol]struct{} {
	l := p.followList(state)
	m := make(map[*y.Symbol]struct{}, len(l))
	for _, v := range l {
		m[v] = struct{}{}
	}
	return m
}

func (p *yparser) action(state int, sym *y.Symbol) *y.Action {
	for _, v := range p.Table[state] {
		if v.Sym == sym {
			return &v
		}
	}
	return nil
}

func testParserYacc(t *testing.T, files []string) {
	var cover map[int]struct{}
	var yl *ylex
	yp := newYParser(
		func(rule int) {
			if rule == yl.lbraceRule {
				yl.fixLbr()
			}
		},
		func(state int) { delete(cover, state) },
	)
	cover = yp.newCover()
	cn0 := len(cover)
	l := NewLexer(nil, nil)
	yl = newYlex(l, yp)
	sum := 0
	toks := 0
	nfiles := 0
	fset := token.NewFileSet()
	for _, path := range files {
		src, err := ioutil.ReadFile(path)
		if err != nil {
			t.Fatal(err)
		}

		nfiles++
		sum += len(src)
		yl.init(fset.AddFile(path, -1, len(src)), src)
		var pos token.Position
		if err = yp.parse(
			func(int) (s *y.Symbol) {
				pos, s = yl.lex()
				toks++
				return s
			},
		); err != nil {
			t.Fatalf("%s: %v", pos, err)
		}
	}
	if cn := len(cover); cn != 0 {
		t.Errorf("states covered: %d/%d", cn0-cn, cn0)
	} else {
		t.Logf("states covered: %d/%d", cn0-cn, cn0)
	}
	t.Logf("files: %v, toks: %v, bytes %v", nfiles, toks, sum)
	e := -1
	for s := range cover {
		if e < 0 || e > s {
			e = s
		}
	}
	if e >= 0 {
		t.Errorf("states %v, unused %v, first unused state %v", len(yp.States), len(cover), e)
	}
}

func (p *parser) fail(nm string) string {
	var yl *ylex
	var states []int
	yp := newYParser(
		func(rule int) {
			if rule == yl.lbraceRule {
				yl.fixLbr()
			}
		},
		func(st int) { states = append(states, st) },
	)
	yl = newYlex(NewLexer(nil, nil), yp)
	fset := token.NewFileSet()
	yl.init(fset.AddFile("", -1, len(p.l.src)), p.l.src)
	yp.parse(
		func(st int) *y.Symbol {
			if pos, s := yl.lex(); pos.Offset <= int(p.off) {
				return s
			}

			return yp.Syms["$end"]
		},
	)
	return yp.report(states[mathutil.Max(0, len(states)-12):])
}

func (p *parser) todo() {
	_, fn, fl, _ := runtime.Caller(1)
	p.err(p.position(), "%q=%q: TODO %v:%v", p.c, p.l.lit, fn, fl) //TODOOK
}

func newTestContext() (*Context, error) {
	a := strings.Split(os.Getenv("GOPATH"), string(os.PathListSeparator)) //TODO Handle unset $GOPATH in go1.8. (?)
	for i, v := range a {
		a[i] = filepath.Join(v, "src")
	}
	return NewContext(runtime.GOOS, runtime.GOARCH, VersionTags(), append([]string{filepath.Join(runtime.GOROOT(), "src")}, a...))
}

func testParser(t *testing.T, packages []*Package) {
	ctx, err := newTestContext()
	if err != nil {
		t.Fatal(err)
		return
	}

	errorList := newErrorList(0)
	for _, v := range packages {
		ctx.load(
			token.Position{},
			v.ImportPath,
			func(p *parser) {
				p.err(p.position(), "syntax error\n----\n%s", p.fail(p.sourceFile.Path))
			},
			errorList,
		).waitFor()
		if err := errorList.error(); err != nil {
			t.Fatal(err)
		}
	}
	t.Logf("packages: %v", len(packages))
}

func testParserRejectFS(t *testing.T) {
	ctx, err := newTestContext()
	if err != nil {
		t.Fatal(err)
	}

	ctx.ignoreImports = true
	yp := newYParser(nil, nil)
	l := NewLexer(nil, nil)
	cases := 0
	fset := token.NewFileSet()
	for state, s := range yp0.States {
		syms, _ := s.Syms0()
		var a []string
		for _, sym := range syms {
			a = append(a, yp.sym2str(sym))
		}
		s0 := strings.Join(a, " ") + " "
		fs := yp.followSet(state)
		for _, sym := range yp.Syms {
			if !sym.IsTerminal {
				continue
			}

			if _, ok := fs[sym]; ok {
				continue
			}

			s := s0 + yp.sym2str(sym) + "@"
			l.init(fset.AddFile("", -1, len(s)), []byte(s))
			pkg := newPackage(ctx, "", "", newErrorList(-1))
			sf := newSourceFile(pkg, "", nil, nil)
			p := newParser(sf, l)
			p.ignoreRedeclarations = true
			off := int32(-1)
			p.syntaxError = func(*parser) {
				if off < 0 {
					off = p.off
				}
			}
			cases++
			p.file()
			if off < 0 {
				t.Fatalf(`%d: "%s" unexpected success, final sym %q`, state, s, sym)
			}

			if g, e := off, int32(len(s0)); g < e {
				t.Fatalf(`Follow set %v
state %3d: %s unexpected error position, got %v expected %v
           %s^`, yp.followList(state), state, s, g+1, e+1, strings.Repeat("-", int(g)))
			}
		}
	}
	t.Logf("test cases: %v", cases)
}

func quoteErrMessage(s string) string {
	return strings.Replace(strings.Replace(s, "\t", "\\t", -1), "\n", "\\n", -1)
}

type errchk struct {
	re  []byte
	pos token.Position
}

type errchks []errchk

func (e *errchks) comment(position token.Position, s []byte) {
	if bytes.HasPrefix(s, generalCommentStart) {
		s = s[len(generalCommentStart):]
	}
	if bytes.HasSuffix(s, generalCommentEnd) {
		s = s[:len(s)-len(generalCommentEnd)]
	}
	s = bytes.TrimSpace(s)
	n := len(errCheckMark1)
	i := bytes.LastIndex(s, errCheckMark1)
	j := bytes.LastIndex(s, errCheckDisbled)
	if i < 0 {
		i = bytes.LastIndex(s, errCheckMark2)
		n = len(errCheckMark2)
		if i < 0 {
			return // No check found.
		}
	}

	if j >= 0 && j < i {
		return // Check disabled.
	}

	s = s[i+n:]
	s = bytes.TrimSpace(s)
	*e = append(*e, errchk{s, position})
}

func (e errchks) errors(t *testing.T, err scanner.ErrorList, fname string, syntaxOnly bool) {
	err.Sort()
	if *oRE != "" {
		for _, v := range e {
			t.Log(v)
		}
		if len(e) != 0 {
			t.Logf("FAIL\n%s", errString(err))
		}
	}

	got := map[int][]*scanner.Error{}
	var gota []int
	for _, v := range err {
		p := filepath.ToSlash(v.Pos.Filename)
		if !filepath.IsAbs(p) {
			line := v.Pos.Line
			got[line] = append(got[line], v)
			gota = append(gota, line)
		}
	}
	gota = gota[:sortutil.Dedupe(sort.IntSlice(gota))]

	expect := map[int]errchk{}
	for _, v := range e {
		expect[v.pos.Line] = v
	}

	var a scanner.ErrorList
	var fail bool
outer:
	for _, line := range gota {
		matched := false
		var g0, g *scanner.Error
		var e errchk
	inner:
		for _, g = range got[line] {
			if g0 == nil {
				g0 = g
			}
			var ok bool
			if e, ok = expect[line]; !ok {
				a = append(a, &scanner.Error{Pos: g.Pos, Msg: fmt.Sprintf("[FAIL errorcheck: extra error] %s", quoteErrMessage(g.Msg))})
				fail = true
				continue outer
			}

			for _, v := range errCheckPatterns.FindAllSubmatch(e.re, -1) {
				re := v[1]
				ok, err := regexp.MatchString(string(re), strings.SplitN(g.Error(), ": ", 2)[1])
				if err != nil {
					t.Fatal(err)
				}

				if ok {
					if !syntaxOnly {
						a = append(a, &scanner.Error{Pos: g.Pos, Msg: fmt.Sprintf("[PASS errorcheck] %s: %s", e.re, quoteErrMessage(g.Msg))})
					}
					matched = true
					break inner
				}
			}
		}
		if !matched && !syntaxOnly {
			a = append(a, &scanner.Error{Pos: g.Pos, Msg: fmt.Sprintf("[FAIL errorcheck: error does not match] %s: %s", e.re, quoteErrMessage(g0.Msg))})
			fail = true
		}
		delete(expect, line)
	}
	if !fail && (len(expect) == 0 || syntaxOnly) {
		//t.Logf("[PASS errorcheck] %v\n", fname)
	}
	if !syntaxOnly {
		for _, e := range expect {
			a = append(a, &scanner.Error{Pos: e.pos, Msg: fmt.Sprintf("[FAIL errorcheck: missing error] %s", e.re)})
		}
	}
	a.Sort()
	if len(a) != 0 {
		t.Fatalf("\n%s", errString(a))
	}
}

// Verify no syntax error are reported for lines w/o the magic [GC_]ERROR comments.
func testParserErrchk(t *testing.T) {
	ctx, err := newTestContext()
	if err != nil {
		t.Fatal(err)
	}

	var (
		checks errchks
		errors scanner.ErrorList
		l      = NewLexer(nil, nil)
	)

	ctx.ignoreImports = true
	l.CommentHandler = func(off int32, lit []byte) {
		checks.comment(l.position(off), lit)
	}
	fset := token.NewFileSet()
	for _, fn := range errchkFiles {
		src, err := ioutil.ReadFile(fn)
		if err != nil {
			t.Fatal(err)
		}

		l.init(fset.AddFile(fn, -1, len(src)), src)
		pkg := newPackage(ctx, "", "", newErrorList(-1))
		sf := newSourceFile(pkg, fn, nil, nil)
		p := newParser(sf, l)
		p.syntaxError = func(*parser) {
			// Whitelist cases like
			//	testdata/errchk/gc/syntax/semi1.go:14:2: [FAIL errorcheck: extra error] syntax error, lookahead "EOF"=""
			if p.c == token.EOF || p.l.off+1 >= int32(len(p.l.src))-1 {
				return
			}

			errors.Add(p.position(), fmt.Sprintf("syntax error, lookahead %q=%q, p.l.off %d/%d", p.c, p.l.lit, p.l.off, len(p.l.src)))
		}
		errors = errors[:0]
		checks = checks[:0]
		p.file()
		for l.c != classEOF {
			l.Scan()
		}
		checks.errors(t, errors, fn, true)
	}
	t.Logf("files: %v", len(errchkFiles))
}

func TestParser(t *testing.T) {
	cover := append(gorootFiles, yaccCover)
	coverPackages := append(gorootPackages, newPackage(nil, filepath.Join(selfImportPath, filepath.Dir(yaccCover)), "", nil))
	_ = t.Run("Yacc", func(t *testing.T) { testParserYacc(t, cover) }) && //TODOOK
		t.Run("GOROOT", func(t *testing.T) { testParser(t, coverPackages) }) &&
		t.Run("RejectFollowSet", testParserRejectFS) &&
		t.Run("Errchk", testParserErrchk)
}
