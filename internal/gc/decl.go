// Copyright 2016 The GC Authors. All rights reservedGG.
// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.

package gc

import (
	"fmt"
	"go/token"
)

var (
	_ Declaration = (*ConstDecl)(nil)
	_ Declaration = (*FuncDecl)(nil)
	_ Declaration = (*ImportSpec)(nil)
	_ Declaration = (*MethodDecl)(nil)
	_ Declaration = (*TypeDecl)(nil)
	_ Declaration = (*VarDecl)(nil)
)

// ------------------------------------------------------------------- Bindings

// Bindings map names to Declarations.
type Bindings map[string]Declaration

func (b *Bindings) declare(p *parser, d Declaration) {
	if *b == nil {
		*b = Bindings{}
	}
	m := *b
	nm := d.Name()
	if nm == "" { //TODO-
		panic(fmt.Errorf("%s: internal error: Bindings.declare - empty declaration name", p.l.file.Position(d.Pos())))
	}

	ex := m[nm]
	if ex == nil {
		m[nm] = d
		return
	}

	if p.sourceFile.Package.ctx.tweaks.ignoreRedeclarations {
		return
	}

	p.err(
		p.l.file.Position(d.Pos()),
		"%v redeclared in this block\n\tprevious declaration at %v",
		d.Name(),
		p.sourceFile.Package.ctx.FileSet.File(ex.Pos()).Position(ex.Pos()),
	)
}

// ---------------------------------------------------------------- declaration

type declaration struct {
	Token
	visibility token.Pos
}

// Const implements Declaration.
func (d *declaration) Const() *ConstDecl { panic("Const of inappropriate Declaration") }

// Func implements Declaration.
func (d *declaration) Func() *FuncDecl { panic("Func of inappropriate Declaration") }

// ImportSpec implements Declaration.
func (d *declaration) ImportSpec() *ImportSpec { panic("ImportSpec of inappropriate Declaration") }

// Method implements Declaration.
func (d *declaration) Method() *MethodDecl { panic("Method of inappropriate Declaration") }

// Name implements Declaration.
func (d *declaration) Name() string { return d.Val }

// Pos implements Declaration.
func (d *declaration) Pos() token.Pos { return d.Token.Pos }

// Type implements Declaration.
func (d *declaration) Type() *TypeDecl { panic("Type of inappropriate Declaration") }

// Var implements Declaration.
func (d *declaration) Var() *VarDecl { panic("Var of inappropriate Declaration") }

// Visibility implements Declaration.
func (d *declaration) Visibility() token.Pos { return d.visibility }

// ---------------------------------------------------------------------- Scope

// Scope tracks declarations.
type Scope struct {
	Bindings Bindings
	Kind     ScopeKind
	Labels   Bindings
	Parent   *Scope
	Unbound  []Declaration // Declarations named _.
	pkg      *Package      // Only for Package scope.
}

func newScope(kind ScopeKind, parent *Scope) *Scope {
	return &Scope{
		Kind:   kind,
		Parent: parent,
	}
}

func (s *Scope) declare(p *parser, d Declaration) {
	nm := d.Name()
	if s.Kind == FileScope {
		p.sourceFile.TopLevelDecls = append(p.sourceFile.TopLevelDecls, d)
	}
	if nm == "_" {
		if s.Kind != FileScope {
			s.Unbound = append(s.Unbound, d)
		}
		return
	}
	switch d.(type) {
	case *ConstDecl, *VarDecl, *TypeDecl, *FuncDecl:
		s.Bindings.declare(p, d)
	case *ImportSpec:
		if s.Kind != FileScope { //TODO-
			panic("internal error")
		}

		pkg := p.sourceFile.Package
		switch ex, ok := pkg.fileScopeNames[nm]; {
		case ok:
			_ = ex
			panic(p.pos())
			//TODO p.todo()
		default:
			s.Bindings.declare(p, d)
		}
	default:
		panic("internal error")
	}
}

// Lookup searches for nm in s or any of its parents. fileScope, if not nil, is
// searched  prior to searching the universe scope when lookup does not find nm
// in package scope and s belongs to pkg.
func (s *Scope) Lookup(pkg *Package, fileScope *Scope, nm Token) Declaration {
	for s0 := s; s != nil; s = s.Parent {
		if d, ok := s.Bindings[nm.Val]; ok && (s.Kind != BlockScope || s != s0 || d.Visibility() < nm.Pos) {
			return d
		}

		if s.Kind == PackageScope && s.pkg == pkg && fileScope != nil {
			if d, ok := fileScope.Bindings[nm.Val]; ok {
				return d
			}
		}
	}
	return nil
}

// ConstDecl describes a constant declaration.
type ConstDecl struct {
	declaration
}

func newConstDecl(tok Token, visibility token.Pos) *ConstDecl {
	return &ConstDecl{
		declaration: declaration{tok, visibility},
	}
}

// Const implements Declaration.
func (d *ConstDecl) Const() *ConstDecl { return d }

// Kind implements Declaration.
func (d *ConstDecl) Kind() DeclarationKind { return ConstDeclaration }

// FuncDecl describes a function declaration.
type FuncDecl struct {
	declaration
}

func newFuncDecl(tok Token, visibility token.Pos) *FuncDecl {
	return &FuncDecl{
		declaration: declaration{tok, visibility},
	}
}

// Func implements Declaration.
func (d *FuncDecl) Func() *FuncDecl { return d }

// Kind implements Declaration.
func (d *FuncDecl) Kind() DeclarationKind { return FuncDeclaration }

// MethodDecl describes a method declaration.
type MethodDecl struct {
	declaration
}

func newMethodDecl(tok Token, visibility token.Pos) *MethodDecl {
	return &MethodDecl{
		declaration: declaration{tok, visibility},
	}
}

// Kind implements Declaration.
func (d *MethodDecl) Kind() DeclarationKind { return MethodDeclaration }

// Method implements Declaration.
func (d *MethodDecl) Method() *MethodDecl { return d }

// TypeDecl describes a type declaration.
type TypeDecl struct {
	declaration
}

func newTypeDecl(tok Token, visibility token.Pos) *TypeDecl {
	return &TypeDecl{
		declaration: declaration{tok, visibility},
	}
}

// Kind implements Declaration.
func (d *TypeDecl) Kind() DeclarationKind { return TypeDeclaration }

// Type implements Declaration.
func (d *TypeDecl) Type() *TypeDecl { return d }

// VarDecl describes a variable declaration.
type VarDecl struct {
	declaration
}

func newVarDecl(tok Token, visibility token.Pos) *VarDecl {
	return &VarDecl{
		declaration: declaration{tok, visibility},
	}
}

// Kind implements Declaration.
func (d *VarDecl) Kind() DeclarationKind { return VarDeclaration }

// Var implements Declaration.
func (d *VarDecl) Var() *VarDecl { return d }

// Declaration describes a constant, function, method, type or variable
// declaration.
type Declaration interface {
	Node

	// Const returns the Declaration's *ConstDecl. It panics if Kind is not
	// ConstDeclaration.
	Const() *ConstDecl

	// Func returns the Declaration's  *FuncDecl. It panics if Kind is not
	// FuncDeclaration.
	Func() *FuncDecl

	// ImportSpec returns the Declaration's  *ImportSpec. It panics if Kind
	// is not ImportDeclaration.
	ImportSpec() *ImportSpec

	// Kind returns the Declarations's kind.
	Kind() DeclarationKind

	// Method returns the Declaration's  *MethodDecl. It panics if Kind is
	// not MethodDeclaration.
	Method() *MethodDecl

	// Name returns the declared name.
	Name() string

	// Type returns the Declaration's  *TypeDecl. It panics if Kind is not
	// TypeDeclaration.
	Type() *TypeDecl

	// Var returns the Declaration's  *VarDecl. It panics if Kind is not
	// VarDeclaration.
	Var() *VarDecl

	// Visibility returns the position at which the declaration is visible
	// in its declaration scope or token.NoPos for declarations in package
	// and file scope.
	Visibility() token.Pos
}
