// Copyright 2016 The Browse Authors. All rights reserved.
// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.

package main

import (
	"github.com/cznic/browse/internal/gc"
	"github.com/cznic/wm"
	"github.com/gdamore/tcell"
	"os"
	"runtime"
)

var (
	windowStyle8 = wm.WindowStyle{
		Border:     wm.Style{Background: tcell.ColorNavy, Foreground: tcell.ColorGreen},
		ClientArea: wm.Style{Background: tcell.ColorSilver, Foreground: tcell.ColorBlack},
		Title:      wm.Style{Background: tcell.ColorNavy, Foreground: tcell.ColorSilver},
	}

	theme8 = &wm.Theme{
		ChildWindow: windowStyle8,
		Desktop: wm.WindowStyle{
			ClientArea: wm.Style{Background: tcell.ColorTeal, Foreground: tcell.ColorWhite},
		},
	}

	windowStyle256 = wm.WindowStyle{
		Border:     wm.Style{Background: tcell.NewHexColor(0xdedcda), Foreground: tcell.NewHexColor(0x86abd9)},
		ClientArea: wm.Style{Background: tcell.NewHexColor(0xefefef), Foreground: tcell.NewHexColor(0x222222)},
		Title:      wm.Style{Background: tcell.NewHexColor(0x86abd9), Foreground: tcell.NewHexColor(0x222222)},
	}

	theme256 = &wm.Theme{
		ChildWindow: windowStyle256,
		Desktop: wm.WindowStyle{
			ClientArea: wm.Style{Background: tcell.NewHexColor(0xa58132), Foreground: tcell.NewHexColor(0xffffff)},
		},
	}
)

type browser struct {
	ctx       *gc.Context
	files     map[string]*file
	newWinPos wm.Position
	pkg       *gc.Package
}

func newBrowser(ctx *gc.Context) *browser {
	return &browser{
		ctx:       ctx,
		files:     map[string]*file{},
		newWinPos: wm.Position{X: 2, Y: 2},
	}
}

func (b *browser) run(pkg *gc.Package) (err error) {
	var colors int
	switch {
	case runtime.GOOS == "windows":
		colors = 16
	default:
		info, err := tcell.LookupTerminfo(os.Getenv("TERM"))
		if err != nil {
			return err
		}

		colors = info.Colors
	}
	theme := theme256
	if colors < 256 {
		theme = theme8
	}
	if app, err = wm.NewApplication(theme); err != nil {
		return err
	}

	b.pkg = pkg
	return app.Run(b.setup)
}

func (b *browser) incNewWinPos() {
	b.newWinPos.X += 3
	if b.newWinPos.X > app.Desktop().Root().Size().Width/2 {
		b.newWinPos.X = 2
	}
	b.newWinPos.Y += 3
	if b.newWinPos.Y > app.Desktop().Root().Size().Height/2 {
		b.newWinPos.Y = 2
	}
}

func (b *browser) openFile(area wm.Rectangle, sf *gc.SourceFile) *file {
	f := b.files[sf.Path]
	if f == nil {
		f = newFile(b, area, sf)
	}
	return f
}

func (b *browser) onKey(w *wm.Window, prev wm.OnKeyHandler, key tcell.Key, mod tcell.ModMask, r rune) bool {
	if prev != nil && prev(w, nil, key, mod, r) {
		return true
	}

	switch key {
	case tcell.KeyCtrlQ:
		app.Exit(nil)
		return true
	}

	return false
}

func (b *browser) setup() {
	app.SetDoubleClickDuration(0)
	app.SetDesktop(app.NewDesktop())
	app.OnKey(b.onKey, nil)
	var f *file
	for _, v := range b.pkg.SourceFiles {
		f = b.openFile(wm.Rectangle{Position: b.newWinPos, Size: wm.Size{Width: 80, Height: 24}}, v)
		b.incNewWinPos()
	}
	if f != nil {
		f.BringToFront()
		f.SetFocus(true)
	}
}
