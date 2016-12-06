// Copyright 2016 The Browse Authors. All rights reserved.
// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.

//TODO $ browse runtime # Takes 10+ seconds to show on r550.
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

type comment struct {
	column int32
	len    int32
}

type file struct {
	*tk.View
	browser      *browser
	lnDigits     int
	lnOffsets    []int
	lnStyle      wm.Style
	sf           *gc.SourceFile
	src          []byte
	style        wm.Style
	commentStyle wm.Style
	comments     map[int32][]comment // line: comments
}

func newFile(b *browser, area wm.Rectangle, sf *gc.SourceFile) *file {
	color := tcell.NewHexColor(0x999999)
	commentColor := tcell.NewHexColor(0x0e6e2c)
	if app.Colors() < 256 {
		color = tcell.ColorRed
		commentColor = tcell.ColorGreen
	}
	style := app.ChildWindowStyle().ClientArea
	f := &file{
		browser:      b,
		commentStyle: wm.Style{Foreground: commentColor, Background: style.Background},
		comments:     map[int32][]comment{},
		lnStyle:      wm.Style{Foreground: color, Background: style.Background},
		sf:           sf,
		style:        style,
	}

	var err error
	if f.src, err = ioutil.ReadFile(sf.Path); err != nil {
		app.Exit(err)
		return nil
	}

	lx := gc.NewLexer(f.src)
	lx.CommentHandler = f.commentHandler
	for tok := token.Token(-1); tok != token.EOF; _, _, _, tok = lx.Scan() {
	}
	f.View = tk.NewView(app.Desktop().Root().NewChild(wm.Rectangle{Position: area.Position}), f)
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
	f.OnPaintClientArea(f.onPaint, nil)
	f.SetCloseButton(true)
	f.SetSize(area.Size)
	f.SetTitle(sf.Path)
	return f
}

func (f *file) commentHandler(pos gc.Position, lit []byte) {
	f.comments[pos.Line] = append(f.comments[pos.Line], comment{pos.Column, int32(len(lit))})
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
		x := 0
		for _, v := range f.comments[int32(line+1)] {
			pre := src[:v.column]
			src = src[v.column:]
			w, s := f.displayString(pre)
			f.Printf(f.lnDigits+1+x, line, f.style, "%s", s)
			x += w
			comment := src[:v.len]
			src = src[v.len:]
			w, s = f.displayString(comment)
			f.Printf(f.lnDigits+1+x, line, f.commentStyle, "%s", s)
			x += w
		}
		if len(src) != 0 {
			_, s := f.displayString(src)
			f.Printf(f.lnDigits+1+x, line, f.style, "%s", s)
		}
	}
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
