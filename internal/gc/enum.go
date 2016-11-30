// Copyright 2016 The GC Authors. All rights reserved.
// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.

package gc

// DeclarationKind describes a Declaration's Kind.
type DeclarationKind int

// Values of DeclarationKind.
const (
	ConstDeclaration DeclarationKind = iota
	FuncDeclaration
	ImportDeclaration
	MethodDeclaration
	TypeDeclaration
	VarDeclaration
)

// ScopeKind describe a Scope's Kind.
type ScopeKind int

// Values of ScopeKind.
const (
	UniverseScope ScopeKind = iota
	PackageScope
	FileScope
	BlockScope
)
