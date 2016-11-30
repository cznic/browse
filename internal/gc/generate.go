// Copyright 2016 The GC Authors. All rights reserved.
// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.

// +build ignore

package main

import (
	"fmt"
	"log"
	"os"
	"path/filepath"
)

func main() {
	const lex = "testdata/fuzz/lexer/"
	for i := 0; i < 512; i++ {
		f, err := os.Create(filepath.Join(lex, fmt.Sprintf("%d.go", i)))
		if err != nil {
			log.Fatal(err)
		}

		if _, err = f.WriteString(string(i)); err != nil {
			log.Fatal(err)
		}

		if err = f.Close(); err != nil {
			log.Fatal(err)
		}
	}
}
