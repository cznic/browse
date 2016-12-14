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
	"time"
)

const (
	logo       = "http://github.com/cznic/browse"
	logoBorder = 1

	defaultWindowWidth  = 80
	defaultWindowHeight = 24
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
	desktop   *wm.Desktop
	files     int
	howerWin  *file
	logoStyle wm.Style
	newWinPos wm.Position
	pkg       *gc.Package
	theme     *wm.Theme
}

func newBrowser(ctx *gc.Context) *browser {
	return &browser{
		ctx:       ctx,
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
	b.theme = theme256
	if colors < 256 {
		b.theme = theme8
	}

	b.logoStyle = wm.Style{Background: b.theme.Desktop.ClientArea.Background, Foreground: tcell.ColorWhite}
	if app, err = wm.NewApplication(b.theme); err != nil {
		return err
	}

	b.desktop = app.NewDesktop()
	b.pkg = pkg
	return app.Run(b.setup)
}

func (b *browser) incNewWinPos() {
	b.newWinPos.X += 3
	if b.newWinPos.X > b.desktop.Root().Size().Width/2 {
		b.newWinPos.X = 2
	}
	b.newWinPos.Y += 3
	if b.newWinPos.Y > b.desktop.Root().Size().Height/2 {
		b.newWinPos.Y = 2
	}
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

func (b *browser) onPaintClientArea(w *wm.Window, prev wm.OnPaintHandler, ctx wm.PaintContext) {
	if prev != nil {
		prev(w, nil, ctx)
	}
	sz := w.Size()
	w.Printf(sz.Width-logoBorder-len(logo), sz.Height-logoBorder-1, b.logoStyle, logo)
	if debug {
		w.Printf(sz.Width-logoBorder-len(logo), sz.Height-logoBorder, b.logoStyle, "%v %p", b.desktop.Root().Rendered(), b.howerWin)
	}
}

func (b *browser) onMouseMove(w *wm.Window, prev wm.OnMouseHandler, button tcell.ButtonMask, screenPos, winPos wm.Position, mods tcell.ModMask) bool {
	if prev != nil && prev(w, nil, button, screenPos, winPos, mods) {
		return true
	}

	b.howerWin.leave()
	return true
}

func (b *browser) newFile(sf *gc.SourceFile) *file {
	f := newFile(b, wm.Rectangle{Position: b.newWinPos, Size: wm.Size{Width: defaultWindowWidth, Height: defaultWindowHeight}}, sf)
	f.Invalidate(f.Area())
	b.incNewWinPos()
	return f
}

func (b *browser) setup() {
	app.SetDoubleClickDuration(0)
	app.OnKey(b.onKey, nil)
	r := b.desktop.Root()
	r.OnPaintClientArea(b.onPaintClientArea, nil)
	r.OnMouseMove(b.onMouseMove, nil)
	var f *file
	for _, v := range b.pkg.SourceFiles {
		f = b.newFile(v)
	}
	if f != nil {
		f.BringToFront()
		f.SetFocus(true)
	}
	if debug {
		go func() {
			for range time.Tick(time.Second / 3) {
				sz := r.Size()
				x := sz.Width - logoBorder - len(logo)
				y := sz.Height - logoBorder
				app.PostWait(func() {
					r.InvalidateClientArea(wm.Rectangle{wm.Position{x, y}, wm.Size{len(logo), 1}})
				})
			}
		}()
	}
	b.desktop.Show()
}
