// Copyright 2016 The GC Authors. All rights reserved.
// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.

// +build go1.5,!go1.6,!go1.7,!go1.8,!go1.9

package gc

// VersionTags returns
//
//	[]string{"go1.1", "go1.2", "go1.3", "go1.4", "go1.5"}
func VersionTags() []string {
	return []string{"go1.1", "go1.2", "go1.3", "go1.4", "go1.5"}
}

var goVersion = "1.5"
