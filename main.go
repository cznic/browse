// Copyright 2016 The Browse Authors. All rights reserved.
// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.

// Command browse is a terminal Go source code browser.
//
// Usage:
//
//	browse args...
//
// Every argument must be a valid import path or a valid file path. For import
// path arguments, all files considered by the build are open in the browser in
// separate windows. For file path arguments the respective files are opened in
// separate windows.
//
// If no non-opt arguments are given on the command line, browse checks if the
// current working directory is under the GOPATH, in which case it opens the
// package files in the current directory.
package main

import (
	"flag"
	"log"
	"os"
	"path/filepath"
	"strings"
)

func main() {
	log.SetFlags(log.Lshortfile)
	flag.Parse()
	switch {
	case flag.NArg() == 0:
		cwd, err := os.Getwd()
		if err != nil {
			log.Fatal(err)
		}

		for _, v := range strings.Split(os.Getenv("GOPATH"), string(os.PathListSeparator)) {
			ip, err := filepath.Rel(filepath.Join(v, "src"), cwd)
			if err == nil {
				log.Fatalf("TODO %q", ip)
				break
			}
		}
	default:
		log.Fatal("TODO")
	}
}
