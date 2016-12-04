// Copyright 2016 The Browse Authors. All rights reserved.
// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.

package main

import (
	"bytes"
	"io/ioutil"

	"github.com/cznic/browse/internal/gc"
	"github.com/cznic/mathutil"
	"github.com/cznic/wm"
	"github.com/cznic/wm/tk"
	"github.com/gdamore/tcell"
)

var nl = []byte{'\n'}

type file struct {
	*tk.View
	browser     *browser
	lineOffsets []int
	sf          *gc.SourceFile
	src         []byte
	widths      []int
}

func newFile(b *browser, area wm.Rectangle, sf *gc.SourceFile) *file {
	f := &file{
		browser: b,
		sf:      sf,
	}

	var err error
	if f.src, err = ioutil.ReadFile(sf.Path); err != nil {
		app.Exit(err)
		return nil
	}

	f.View = tk.NewView(app.Desktop().Root().NewChild(wm.Rectangle{Position: area.Position}), f)
	if bytes.HasSuffix(f.src, nl) {
		f.src = f.src[:len(f.src)-1]
	}
	blines := bytes.Split(f.src, nl)
	f.lineOffsets = make([]int, len(blines)+1)
	f.widths = make([]int, len(blines))
	off := 0
	for i, v := range blines {
		f.lineOffsets[i] = off
		off += len(v) + 1
		w := 0
		for _, v := range v {
			switch v {
			case '\t':
				w += 8 - w%8
			default:
				w++
			}
		}
		f.widths[i] = w
	}
	f.lineOffsets[len(f.lineOffsets)-1] = len(f.src)
	b.files[sf.Path] = f
	f.OnClose(f.onClose, nil)
	f.OnKey(f.onKey, nil)
	f.OnPaintClientArea(f.onPaint, nil)
	f.SetCloseButton(true)
	f.SetSize(area.Size)
	f.SetTitle(sf.Path)
	return f
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
		p := w.Parent()
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

	o := w.Origin()
	switch key {
	case tcell.KeyCtrlD:
		w.SetOrigin(wm.Position{X: o.X, Y: o.Y + f.ClientSize().Height/2})
		return true
	case tcell.KeyCtrlE:
		w.SetOrigin(wm.Position{X: o.X, Y: o.Y + 1})
		return true
	case tcell.KeyCtrlU:
		w.SetOrigin(wm.Position{X: o.X, Y: o.Y - f.ClientSize().Height/2})
		return true
	case tcell.KeyCtrlW:
		f.Close()
		return true
	case tcell.KeyCtrlY:
		w.SetOrigin(wm.Position{X: o.X, Y: o.Y - 1})
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
	for y := viewport.Y; y < viewport.Y+viewport.Height; y++ {
		if y >= len(f.widths) {
			break
		}

		w = mathutil.Max(w, f.widths[y])
	}
	return wm.Size{Width: w, Height: len(f.widths)}
}

func (f *file) onPaint(w *wm.Window, prev wm.OnPaintHandler, ctx wm.PaintContext) {
	if prev != nil {
		prev(w, nil, ctx)
	}
	cpY := w.ClientPosition().Y
	for i := 0; i < ctx.Height; i++ {
		line := ctx.Y - cpY + i
		if line < 0 || line >= len(f.lineOffsets)-1 {
			break
		}

		w.Printf(0, line, w.ClientAreaStyle(), "%s", f.src[f.lineOffsets[line]:f.lineOffsets[line+1]])
	}
}
