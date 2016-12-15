// Copyright 2016 The Browse Authors. All rights reserved.
// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.

// +build !debug.browse

package main

const debug = false

func foo() int { //TODO-
	app := app
	_ = app
	_ = foo()
	app, foo := app, 42
	_ = app
	_ = foo

	var i int
	var s []int
	for i = range s {
		_ = i
	}
	for i := range s {
		_ = i
	}
	for i, j := range s {
		_, _ = i, j
	}
	for i := 0; i < 10; i++ {
		_ = i
	}

	var x interface{}
	switch x := x.(type) {
	case int:
		_ = x
	default:
		_ = x
	}

	switch x := x; x := x.(type) { //TODO 42:17 should link nowhere.
	case int:
		_ = x //TODO should link to 42:17, not 42:9.
	default:
		_ = x //TODO should link to 42:17, not 42:9.
	}

	var c chan int
	select {
	case x = <-c: //TODO 51:13 missing link
		_ = x
		_ = c
		_ = <-c
	case x := <-c: //TODO 55:14 missing link
		_ = x
		_ = c
		_ = <-c
	}

	_ = c
	_ = <-c
	x, y := <-c, 42
	_ = x
	_ = y

	var bar struct {
		bar int
	}

	if bar.bar+bar.bar == bar.bar {
		println(bar.bar)
	}

	panic(-1)
}
