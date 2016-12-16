// Copyright 2016 The Browse Authors. All rights reserved.
// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.

package main

import (
	"bytes"
	"fmt"
	"go/token"
	"io/ioutil"
	"os/exec"
	"strconv"
	"sync"
	"unicode/utf8"

	"github.com/cznic/browse/internal/ftoken"
	"github.com/cznic/browse/internal/gc"
	"github.com/cznic/mathutil"
	"github.com/cznic/wm"
	"github.com/cznic/wm/tk"
	"github.com/gdamore/tcell"
	"github.com/mattn/go-runewidth"
)

var nl = []byte{'\n'}

const (
	spanComment = iota
	spanIdent
)

type span struct {
	column int32 // Byte offset.
	len    int32 // Byte length.
	kind   int32
	xref   int32
}

type location struct {
	sourceFile *gc.SourceFile
	viewport   wm.Position
}

type file struct {
	*tk.View
	browser      *browser
	commentStyle wm.Style
	howerPos     wm.Position
	identStyle   wm.Style
	inFly        map[token.Pos]struct{} // Pending guru requests.
	lineNoWidth  int
	lineStyle    wm.Style
	lines        []int
	next         []location
	prev         []location
	sourceFile   *gc.SourceFile
	spans        map[int32][]span // line: []span
	src          []byte
	style        wm.Style
	target       token.Pos // "url" in bottom border.
	targetStyle  wm.Style
	xrefMu       sync.Mutex
}

func newFile(b *browser, area wm.Rectangle, sourceFile *gc.SourceFile) *file {
	color := tcell.NewHexColor(0x999999)
	commentColor := tcell.NewHexColor(0x0e6e2c)
	if app.Colors() < 256 {
		color = tcell.ColorRed
		commentColor = tcell.ColorGreen
	}
	style := app.ChildWindowStyle().ClientArea
	identStyle := style
	identStyle.Attr = tcell.AttrUnderline
	f := &file{
		browser:      b,
		commentStyle: wm.Style{Foreground: commentColor, Background: style.Background},
		identStyle:   identStyle,
		lineStyle:    wm.Style{Foreground: color, Background: style.Background},
		inFly:        map[token.Pos]struct{}{},
		sourceFile:   sourceFile,
		spans:        map[int32][]span{},
		style:        style,
		targetStyle:  b.theme.ChildWindow.Border,
	}

	var err error
	if f.src, err = ioutil.ReadFile(sourceFile.Path); err != nil {
		app.Exit(err)
		return nil
	}

	lx := gc.NewLexer(ftoken.NewFileSet().AddFile(sourceFile.Path, -1, len(f.src)), f.src)
	sfi := f.sourceFile.File
	lx.CommentHandler = func(off int32, lit []byte) {
		pos := sfi.Pos(int(off))
		f.commentHandler(sfi.Position(pos), lit)
	}
scan:
	for {
		switch off, t := lx.Scan(); t {
		case token.IDENT:
			tok := lx.Token(off)
			pos := sfi.Pos(int(off))
			position := sfi.PositionFor(pos, false)
			f.spans[int32(position.Line)] = append(
				f.spans[int32(position.Line)],
				span{int32(position.Column) - 1, int32(len(tok.Val)), spanIdent, int32(pos)},
			)
		case token.EOF:
			break scan
		}
	}

	if sourceFile.Xref == nil {
		xref := map[token.Pos]token.Pos{}
		pkg := sourceFile.Package
		fileScope := sourceFile.Scope
		for tok, scope := range sourceFile.Xref0 {
			if scope == nil {
				continue
			}

			if d := scope.Lookup(pkg, fileScope, tok); d != nil && d.Pos() != tok.Pos {
				xref[tok.Pos] = d.Pos()
			}
		}
		sourceFile.Xref0 = nil
		sourceFile.Xref = xref
	}

	f.View = tk.NewView(f.browser.desktop.Root().NewChild(wm.Rectangle{Position: area.Position}), f)
	if bytes.HasSuffix(f.src, nl) {
		f.src = f.src[:len(f.src)-1]
	}
	blines := bytes.Split(f.src, nl)
	n := 1
	for nlines := len(blines); nlines > 9; nlines /= 10 {
		n++
	}
	f.lineNoWidth = n
	f.lines = sourceFile.File.Lines()
	f.OnClick(f.onClick, nil)
	f.OnClose(f.onClose, nil)
	f.OnKey(f.onKey, nil)
	f.OnMouseMove(f.onMouseMove, nil)
	f.OnPaintBorderBottom(f.onPaintBorderBottom, nil)
	f.OnPaintClientArea(f.onPaint, nil)
	f.SetCloseButton(true)
	f.SetSize(area.Size)
	f.SetTitle(sourceFile.Path)
	f.browser.files++
	return f
}

func (f *file) commentHandler(position token.Position, lit []byte) {
	for _, v := range bytes.Split(lit, nl) {
		f.spans[int32(position.Line)] = append(
			f.spans[int32(position.Line)],
			span{int32(position.Column) - 1, int32(len(v)), spanComment, 0},
		)
		position.Line++
		position.Column = 1
	}
}

func (f *file) onClose(w *wm.Window, prev wm.OnCloseHandler) {
	if prev != nil {
		prev(w, nil)
	}

	f.browser.files--
	if f.browser.files == 0 {
		app.Exit(nil)
		return
	}

	app.PostWait(func() {
		p := f.Parent()
		w := p.Child(p.Children() - 1)
		w.BringToFront()
		w.SetFocus(true)
	})
}

func (f *file) onKey(w *wm.Window, prev wm.OnKeyHandler, key tcell.Key, mod tcell.ModMask, r rune) bool {
	if prev != nil && prev(w, nil, key, mod, r) {
		return true
	}

	if !f.Focus() {
		return false
	}

	o := f.Origin()
	switch key {
	case tcell.KeyCtrlD:
		f.SetOrigin(wm.Position{X: o.X, Y: o.Y + f.ClientSize().Height/2})
		return true
	case tcell.KeyCtrlE:
		f.SetOrigin(wm.Position{X: o.X, Y: o.Y + 1})
		return true
	case tcell.KeyTAB:
		if n := len(f.next); n != 0 {
			loc := f.next[n-1]
			next := f.next[:n-1]
			prev := append(f.prev, f.location())
			f.exec(loc, prev, next)
		}
		return true
	case tcell.KeyCtrlO:
		if n := len(f.prev); n != 0 {
			loc := f.prev[n-1]
			prev := f.prev[:n-1]
			next := append(f.next, f.location())
			f.exec(loc, prev, next)
		}
		return true
	case tcell.KeyCtrlU:
		f.SetOrigin(wm.Position{X: o.X, Y: o.Y - f.ClientSize().Height/2})
		return true
	case tcell.KeyCtrlW:
		f.Close()
		return true
	case tcell.KeyCtrlY:
		f.SetOrigin(wm.Position{X: o.X, Y: o.Y - 1})
		return true
	case tcell.KeyHome:
		f.Home()
		return true
	case tcell.KeyEnd:
		f.End()
		return true
	case tcell.KeyPgDn, tcell.KeyCtrlF:
		f.PageDown()
		return true
	case tcell.KeyPgUp:
		f.PageUp()
		return true
	}

	return false
}

func (f *file) Metrics(viewport wm.Rectangle) wm.Size {
	w := -1
	for i := 0; i < viewport.Height; i++ {
		line := viewport.Y + i
		if line < 0 || line >= len(f.lines)-1 {
			break
		}

		dw := f.displayWidth(0, f.src[f.lines[line]:f.lines[line+1]])
		w = mathutil.Max(w, f.lineNoWidth+dw)
	}
	return wm.Size{Width: w, Height: len(f.lines) - 1}
}

var sep = []byte(": defined here as ")

func (f *file) guru(key token.Pos, line, lineOff, lineLen int) {
	f.xrefMu.Lock()
	if _, ok := f.inFly[key]; ok {
		f.xrefMu.Unlock()
		return
	}

	f.inFly[key] = struct{}{}
	f.xrefMu.Unlock()

	var target token.Pos
	defer func() {
		f.xrefMu.Lock()
		delete(f.inFly, key)
		f.sourceFile.Xref[key] = target
		f.xrefMu.Unlock()
		if target.IsValid() {
			app.PostWait(func() { f.setHowerPos(f.setHowerPos(wm.Position{-1, -1})) })
		}
	}()

	off := f.lines[line] + lineOff
	out, err := exec.Command(f.browser.guru, "definition", fmt.Sprintf("%s:#%d", f.sourceFile.Path, off)).Output()
	if err != nil {
		return
	}

	i := bytes.Index(out, sep)
	if i < 0 {
		return
	}

	out = out[:i]
	i = bytes.LastIndex(out, []byte{':'})
	tCol, err := strconv.Atoi(string(out[i+1:]))
	if err != nil {
		return
	}

	out = out[:i]
	i = bytes.LastIndex(out, []byte{':'})
	tLine, err := strconv.Atoi(string(out[i+1:]))
	if err != nil {
		return
	}

	sourceFile := f.browser.ctx.SourceFileForPath(string(out[:i]))
	if sourceFile == nil {
		return
	}

	if decl := sourceFile.File.Pos(sourceFile.File.Lines()[tLine-1] + tCol - 1); decl != key {
		target = decl
	}
}

func (f *file) onPaint(w *wm.Window, prev wm.OnPaintHandler, ctx wm.PaintContext) {
	if prev != nil {
		prev(w, nil, ctx)
	}

	cpY := f.ClientPosition().Y
	f.setTarget(token.NoPos)
	for i := 0; i < ctx.Height; i++ {
		line := ctx.Y - cpY + i
		if line < 0 || line >= len(f.lines)-1 {
			break
		}

		f.Printf(0, line, f.lineStyle, "%*d", f.lineNoWidth, line+1)
		src := f.src[f.lines[line]:f.lines[line+1]]
		var col, off int
		hp := f.howerPos
		for _, v := range f.spans[int32(line+1)] {
			span := src[off:v.column]
			off = int(v.column)
			col += f.paintSpan(col, line, f.style, span)
			span = src[v.column : v.column+v.len]
			off += int(v.len)
			switch v.kind {
			case spanComment:
				col += f.paintSpan(col, line, f.commentStyle, span)
			case spanIdent:
			outer:
				switch {
				case hp.Y >= 0 && hp.Y == line && hp.X >= col:
					w := f.displayWidth(col, span)
					if hp.X < col+w {
						pos := token.Pos(v.xref)
						switch target, ok := f.sourceFile.Xref[pos]; {
						case target.IsValid():
							f.setTarget(target)
							col += f.paintSpan(col, line, f.identStyle, span)
							break outer
						case !ok:
							go f.guru(pos, line, int(v.column), len(src))
						}
					}

					fallthrough
				default:
					col += f.paintSpan(col, line, f.style, span)
				}
			default:
				panic("internal error")
			}
		}
		f.paintSpan(col, line, f.style, src[off:])
	}
}

func (f *file) paintSpan(x, y int, style wm.Style, s []byte) int {
	w, s := f.displayString(x, s)
	f.Printf(f.lineNoWidth+1+x, y, style, "%s", s)
	return w
}

func (f *file) displayWidth(col int, s []byte) (w int) {
	w = col
	for len(s) != 0 {
		r, n := utf8.DecodeRune(s)
		switch r {
		case 0, '\r':
			// nop
		case '\t':
			col += 8 - col%8
		default:
			switch runewidth.RuneWidth(r) {
			case 0:
				// nop
			case 1:
				col++
			case 2:
				col += 2
			default:
				panic("internal error")
			}
		}
		s = s[n:]
	}
	return col - w
}

func (f *file) displayString(col int, s []byte) (w int, b []byte) {
	w = col
	for len(s) != 0 {
		r, n := utf8.DecodeRune(s)
		switch r {
		case 0, '\r':
			// nop
		case '\t':
			b = append(b, ' ')
			col++
			for col%8 != 0 {
				b = append(b, ' ')
				col++
			}
		default:
			b = append(b, s[:n]...)
			switch runewidth.RuneWidth(r) {
			case 0:
				// nop
			case 1:
				col++
			case 2:
				w += 2
			default:
				panic("internal error")
			}
		}
		s = s[n:]
	}
	return col - w, b
}

func (f *file) enter() { f.browser.howerWin = f }

func (f *file) leave() {
	if f == nil {
		return
	}

	f.setTarget(token.NoPos)
	f.setHowerPos(wm.Position{-1, -1})
	f.browser.howerWin = nil
}

func (f *file) onMouseMove(w *wm.Window, prev wm.OnMouseHandler, button tcell.ButtonMask, screenPos, winPos wm.Position, mods tcell.ModMask) bool {
	if prev != nil && prev(w, nil, button, screenPos, winPos, mods) {
		return true
	}

	if m := f.browser.howerWin; m != f {
		m.leave()
		f.enter()
	}

	line := winPos.Y
	col := winPos.X - f.lineNoWidth - 1
	if line < 0 || col < 0 {
		f.setTarget(token.NoPos)
		return true
	}

	f.setHowerPos(wm.Position{col, line})
	return true
}

func (f *file) setTarget(new token.Pos) {
	if new == f.target {
		return
	}

	f.target = new
	f.Invalidate(f.BorderBottomArea())
}

func (f *file) setHowerPos(new wm.Position) (old wm.Position) {
	if old = f.howerPos; new == old {
		return old
	}

	o := f.Origin()
	if old.Y >= 0 {
		f.InvalidateClientArea(wm.NewRectangle(o.X, old.Y, o.X+f.ClientSize().Width-1, old.Y))
	}
	f.howerPos = new
	if new.Y >= 0 {
		f.InvalidateClientArea(wm.NewRectangle(o.X, new.Y, o.X+f.ClientSize().Width-1, new.Y))
	}
	return old
}

func (f *file) onPaintBorderBottom(w *wm.Window, prev wm.OnPaintHandler, ctx wm.PaintContext) {
	if prev != nil {
		prev(w, nil, ctx)
	}

	if f.target == token.NoPos {
		return
	}

	fi := f.browser.ctx.FileSet.File(f.target)
	f.Printf(1, f.BorderBottom()-1, f.targetStyle, " %v ", fi.Position(f.target))
}

func (f *file) exec(loc location, prev, next []location) *file {
	g := newFile(f.browser, f.Area(), loc.sourceFile)
	g.prev = prev
	g.next = next
	g.SetPosition(f.Position())
	g.SetSize(f.Size())
	f.Close()
	g.BringToFront()
	g.SetOrigin(loc.viewport)
	g.SetFocus(true)
	return g
}

func (f *file) location() location { return location{f.sourceFile, f.Origin()} }

func (f *file) onClick(w *wm.Window, prev wm.OnMouseHandler, button tcell.ButtonMask, screenPos, winPos wm.Position, mods tcell.ModMask) bool {
	if prev != nil && prev(w, nil, button, screenPos, winPos, mods) {
		return true
	}

	if mods&(tcell.ModAlt|tcell.ModMeta|tcell.ModShift) != 0 {
		return false
	}

	if f.target == token.NoPos {
		return true
	}

	ctx := f.browser.ctx
	fi := ctx.FileSet.File(f.target)
	position := fi.PositionFor(f.target, false)
	sourceFile := ctx.SourceFileForPath(position.Filename)
	switch {
	case button&tcell.Button1 != 0:
		if mods&tcell.ModCtrl == 0 {
			prev := append(f.prev, f.location())
			next := f.next
			f.exec(location{sourceFile, wm.Position{0, position.Line - 1}}, prev, next)
			return true
		}

		fallthrough
	case button&tcell.Button2 != 0:
		f = f.browser.newFile(sourceFile)
		f.BringToFront()
		f.SetOrigin(wm.Position{0, position.Line - 1})
		f.SetFocus(true)
		return true
	}
	return false
}
