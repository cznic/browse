// Copyright 2016 The Browse Authors. All rights reserved.
// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.

//TODO nnn<shift-G>

package main

import (
	"bytes"
	"go/token"
	"io/ioutil"
	"unicode/utf8"

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

type file struct {
	*tk.View
	browser      *browser
	commentStyle wm.Style
	identStyle   wm.Style
	lnDigits     int
	lnOffsets    []int
	lnStyle      wm.Style
	sf           *gc.SourceFile
	spans        map[int32][]span // line: []span
	src          []byte
	style        wm.Style
}

func newFile(b *browser, area wm.Rectangle, sf *gc.SourceFile) *file {
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
		lnStyle:      wm.Style{Foreground: color, Background: style.Background},
		sf:           sf,
		spans:        map[int32][]span{},
		style:        style,
	}

	var err error
	if f.src, err = ioutil.ReadFile(sf.Path); err != nil {
		app.Exit(err)
		return nil
	}

	fset := token.NewFileSet()
	fi := fset.AddFile(sf.Path, -1, len(f.src))
	lx := gc.NewLexer(fi, f.src)
	lx.CommentHandler = func(off int32, lit []byte) {
		f.commentHandler(fi.Position(fi.Pos(int(off))), lit)
	}
scan:
	for {
		switch off, t := lx.Scan(); t {
		case token.IDENT:
			tok := lx.Token(off)
			position := fi.PositionFor(tok.Pos, false)
			f.spans[int32(position.Line)] = append(
				f.spans[int32(position.Line)],
				span{int32(position.Column) - 1, int32(len(tok.Val)), spanIdent, int32(tok.Pos)},
			)
		case token.EOF:
			break scan
		}
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
	f.lnDigits = n
	f.lnOffsets = make([]int, len(blines)+1)
	off := 0
	for i, v := range blines {
		f.lnOffsets[i] = off
		off += len(v) + 1
	}
	f.lnOffsets[len(f.lnOffsets)-1] = len(f.src)
	b.files[sf.Path] = f
	f.OnClose(f.onClose, nil)
	f.OnKey(f.onKey, nil)
	f.OnMouseMove(f.onMouseMove, nil)
	f.OnPaintClientArea(f.onPaint, nil)
	f.SetCloseButton(true)
	f.SetSize(area.Size)
	f.SetTitle(sf.Path)
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

	delete(f.browser.files, f.sf.Path)
	if len(f.browser.files) == 0 {
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
	case tcell.KeyPgDn:
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
		if line < 0 || line >= len(f.lnOffsets)-1 {
			break
		}

		dw := f.displayWidth(f.src[f.lnOffsets[line]:f.lnOffsets[line+1]])
		w = mathutil.Max(w, f.lnDigits+dw)
	}
	return wm.Size{Width: w, Height: len(f.lnOffsets) - 1}
}

func (f *file) onPaint(w *wm.Window, prev wm.OnPaintHandler, ctx wm.PaintContext) {
	if prev != nil {
		prev(w, nil, ctx)
	}

	cpY := f.ClientPosition().Y
	for i := 0; i < ctx.Height; i++ {
		line := ctx.Y - cpY + i
		if line < 0 || line >= len(f.lnOffsets)-1 {
			break
		}

		f.Printf(0, line, f.lnStyle, "%*d", f.lnDigits, line+1)
		src := f.src[f.lnOffsets[line]:f.lnOffsets[line+1]]
		var col, off int
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
				col += f.paintSpan(col, line, f.style, span) //TODO style
			default:
				panic("internal error")
			}
		}
		f.paintSpan(col, line, f.style, src[off:])
	}
}

func (f *file) paintSpan(x, y int, style wm.Style, s []byte) int {
	w, s := f.displayString(s)
	f.Printf(f.lnDigits+1+x, y, style, "%s", s)
	return w
}

func (f *file) displayWidth(s []byte) (w int) {
	for len(s) != 0 {
		r, n := utf8.DecodeRune(s)
		switch r {
		case 0, '\r':
			// nop
		case '\t':
			w += 8 - w%8
		default:
			switch runewidth.RuneWidth(r) {
			case 0:
				// nop
			case 1:
				w++
			case 2:
				w += 2
			default:
				panic("internal error")
			}
		}
		s = s[n:]
	}
	return w
}

func (f *file) displayString(s []byte) (w int, b []byte) {
	for len(s) != 0 {
		r, n := utf8.DecodeRune(s)
		switch r {
		case 0, '\r':
			// nop
		case '\t':
			b = append(b, ' ')
			w++
			for w%8 != 0 {
				b = append(b, ' ')
				w++
			}
		default:
			b = append(b, s[:n]...)
			switch runewidth.RuneWidth(r) {
			case 0:
				// nop
			case 1:
				w++
			case 2:
				w += 2
			default:
				panic("internal error")
			}
		}
		s = s[n:]
	}
	return w, b
}

func (f *file) onMouseMove(w *wm.Window, prev wm.OnMouseHandler, button tcell.ButtonMask, screenPos, winPos wm.Position, mods tcell.ModMask) bool {
	if prev != nil && prev(w, nil, button, screenPos, winPos, mods) {
		return true
	}

	return false
}
