// Copyright 2016 The GC Authors. All rights reserved.
// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.

//TODO declare labels

package gc

import (
	"bytes"
	"fmt"
	"go/token"
	"strconv"
	"strings"
)

var (
	buildMark  = []byte("// +build")
	buildMark2 = []byte("//+build")
)

// Node is implemented by all AST nodes.
type Node interface {
	Pos() token.Pos
}

// Token represents the position and value of a token.
type Token struct {
	Pos token.Pos
	Val string
}

type parser struct {
	c             token.Token
	l             *Lexer
	loophackStack []bool
	scope         *Scope
	sourceFile    *SourceFile
	syntaxError   func(*parser)
	xref          map[Token]*Scope

	off int32

	loophack bool
}

func newParser(src *SourceFile, l *Lexer) *parser {
	p := &parser{
		l:          l,
		sourceFile: src,
	}
	if src != nil {
		p.xref = src.Xref0
	}
	return p
}

func (p *parser) init(src *SourceFile, l *Lexer) {
	p.l = l
	p.loophack = false
	p.loophackStack = p.loophackStack[:0]
	p.sourceFile = src
	p.xref = src.Xref0
}

func (p *parser) err(position token.Position, msg string, args ...interface{}) {
	p.sourceFile.Package.errorList.Add(position, fmt.Sprintf(msg, args...))
}

func (p *parser) n() token.Token {
more:
	switch p.off, p.c = p.l.Scan(); p.c {
	case token.IDENT:
		if p.xref != nil {
			p.xref[p.l.Token(p.off)] = p.scope
		}
	case token.FOR, token.IF, token.SELECT, token.SWITCH:
		p.loophack = true
	case token.LPAREN, token.LBRACK:
		if p.loophack || len(p.loophackStack) != 0 {
			p.loophackStack = append(p.loophackStack, p.loophack)
			p.loophack = false
		}
	case token.RPAREN, token.RBRACK:
		if n := len(p.loophackStack); n != 0 {
			p.loophack = p.loophackStack[n-1]
			p.loophackStack = p.loophackStack[:n-1]
		}
	case token.LBRACE:
		if p.loophack {
			p.c = tokenBODY
			p.loophack = false
		}
	case tokenBOM:
		goto more
	}
	return p.c
}

func (p *parser) tok() Token { return p.l.Token(p.off) }

func (p *parser) opt(tok token.Token) bool {
	if p.c == tok {
		p.n()
		return true
	}

	return false
}

func (p *parser) skip(toks ...token.Token) {
	for p.n() != token.EOF {
		for _, v := range toks {
			if p.c == v {
				return
			}
		}
	}
}

func (p *parser) must(tok token.Token) (ok bool) {
	ok = true
	if p.c != tok {
		p.syntaxError(p)
		ok = false
	}
	p.n()
	return ok
}

func (p *parser) mustTok(tok token.Token) (t Token, ok bool) {
	ok = true
	if p.c != tok {
		p.syntaxError(p)
		p.n()
		return t, false
	}

	t = p.tok()
	p.n()
	return t, true
}

func (p *parser) must2(toks ...token.Token) (ok bool) {
	ok = true
	for _, tok := range toks {
		ok = p.must(tok) && ok
	}
	return ok
}

func (p *parser) not2(toks ...token.Token) bool {
	for _, tok := range toks {
		if p.c == tok {
			return false
		}
	}
	return true
}

func (p *parser) pos() token.Pos           { return p.l.file.Pos(int(p.off)) }
func (p *parser) position() token.Position { return p.l.file.Position(p.pos()) }

func (p *parser) strLit(s string) string {
	value, err := strconv.Unquote(s)
	if err != nil {
		p.err(p.position(), "%s: %q", err, s)
		return ""
	}

	// https://github.com/golang/go/issues/15997
	if s[0] == '`' {
		value = strings.Replace(value, "\r", "", -1)
	}
	return value
}

func (p *parser) commentHandler(_ int32, lit []byte) {
	if p.sourceFile.build {
		if bytes.HasPrefix(lit, buildMark) {
			p.buildDirective(lit[len(buildMark):])
			return
		}

		if bytes.HasPrefix(lit, buildMark2) {
			p.buildDirective(lit[len(buildMark2):])
			return
		}
	}
}

func (p *parser) buildDirective(b []byte) {
	ctx := p.sourceFile.Package.ctx
	s := string(b)
	s = strings.Replace(s, "\t", " ", -1)
	for _, term := range strings.Split(s, " ") { // term || term
		if term = strings.TrimSpace(term); term == "" {
			continue
		}

		val := true
		for _, factor := range strings.Split(term, ",") { // factor && factor
			if factor = strings.TrimSpace(factor); factor == "" {
				continue
			}

			not := factor[0] == '!'
			if not {
				factor = strings.TrimSpace(factor[1:])
			}

			if factor == "" {
				continue
			}

			_, ok := ctx.tags[factor]
			if not {
				ok = !ok
			}

			if !ok {
				val = false
				break
			}

		}
		if val {
			return
		}
	}

	p.sourceFile.build = false
}

func (p *parser) push(s *Scope) {
	s.Parent = p.scope
	p.scope = s
}

func (p *parser) pop() {
	if p.scope.Kind == PackageScope {
		panic("internal error")
	}

	p.scope = p.scope.Parent
}

// ImportSpec is an import declaration.
type ImportSpec struct {
	Dot        bool     // The `import . "foo/bar"` variant is used.
	ImportPath string   // `foo/bar` in `import "foo/bar"`
	Package    *Package // The imported package, if exists.
	Qualifier  string   // `baz` in `import baz "foo/bar"`.
	declaration
}

func newImportSpec(tok Token, off int32, dot bool, qualifier, importPath string) *ImportSpec {
	return &ImportSpec{
		Dot:         dot,
		ImportPath:  importPath,
		Qualifier:   qualifier,
		declaration: declaration{tok, token.NoPos},
	}
}

// ImportSpec implements Declaration.
func (n *ImportSpec) ImportSpec() *ImportSpec { return n }

// Kind implements Declaration.
func (n *ImportSpec) Kind() DeclarationKind { return ImportDeclaration }

// Name implements Declaration.
func (n *ImportSpec) Name() string {
	if n.Qualifier != "" {
		return n.Qualifier
	}

	return n.Package.Name
}

// importSpec:
// 	'.' STRING
// |	IDENT STRING
// |	STRING
func (p *parser) importSpec() {
	var decl, qualifier Token
	var dot bool
	switch p.c {
	case token.IDENT:
		qualifier = p.tok()
		decl = qualifier
		p.n()
	case token.PERIOD:
		dot = true
		p.n()
	}
	switch p.c {
	case token.STRING:
		if decl.Val == "" {
			decl = p.tok()
		}
		ip := p.strLit(string(p.l.lit))
		if ip == "C" { //TODO
			p.n()
			break
		}

		if !p.sourceFile.Package.ctx.tweaks.ignoreImports {
			spec := newImportSpec(decl, p.off, dot, qualifier.Val, ip)
			spec.Package = p.sourceFile.Package.ctx.load(p.position(), ip, nil, p.sourceFile.Package.errorList).waitFor()
			p.sourceFile.ImportSpecs = append(p.sourceFile.ImportSpecs, spec)
			switch {
			case dot:
				//TODO p.todo()
			default:
				if ex, ok := spec.Package.fileScopeNames[spec.Name()]; ok {
					_ = ex
					//TODO p.todo() // declared in pkg and file scope at the same time.
					break
				}

				if spec.Name() != "" {
					p.sourceFile.Scope.declare(p, spec)
				}
			}
		}
		p.n()
	default:
		p.syntaxError(p)
	}
}

// importSpecList:
// 	importSpec
// |	importSpecList ';' importSpec
func (p *parser) importSpecList() {
	for p.importSpec(); p.opt(token.SEMICOLON) && p.c != token.RPAREN; {
		p.importSpec()
	}
}

// imports:
// |	imports "import" '(' ')' ';'
// |	imports "import" '(' importSpecList semiOpt ')' ';'
// |	imports "import" importSpec ';'
func (p *parser) imports() {
	for p.opt(token.IMPORT) {
		switch {
		case p.opt(token.LPAREN):
			if !p.opt(token.RPAREN) {
				p.importSpecList()
				if p.c == token.COMMA {
					p.syntaxError(p)
					p.skip(token.RPAREN)
				}
				p.must(token.RPAREN)
			}
		default:
			p.importSpec()
		}
		p.must(token.SEMICOLON)
	}
}

// identList:
// 	IDENT
// |	identList ',' IDENT
func (p *parser) identList() (l []Token) {
	switch p.c {
	case token.IDENT:
		l = []Token{p.tok()}
		p.n()
		for p.opt(token.COMMA) && p.c != tokenGTGT {
			switch p.c {
			case token.IDENT:
				l = append(l, p.tok())
				p.n()
			default:
				p.syntaxError(p)
			}
		}
	default:
		p.syntaxError(p)
	}
	return l
}

// compLitExpr:
// 	'{' bracedKeyValList '}'
// |	expr
func (p *parser) compLitExpr() /*TODO return value */ {
	if p.opt(token.LBRACE) {
		p.bracedKeyValList()
		p.must(token.RBRACE)
		return
	}

	p.expr()
}

// keyVal:
// 	compLitExpr
// |	compLitExpr ':' compLitExpr
func (p *parser) keyVal() /*TODO return value */ {
	p.compLitExpr()
	if !p.opt(token.COLON) {
		return
	}

	p.compLitExpr()
}

// keyValList:
// 	keyVal
// |	keyValList ',' keyVal
func (p *parser) keyValList() /*TODO return value */ {
	for p.keyVal(); p.opt(token.COMMA) && p.c != token.RBRACE; {
		p.keyVal()
	}
	if p.c == token.SEMICOLON {
		p.syntaxError(p)
		p.n()
	}
}

// bracedKeyValList:
// |	keyValList commaOpt
func (p *parser) bracedKeyValList() /*TODO return value */ {
	if p.c != token.RBRACE {
		p.keyValList()
		p.opt(token.COMMA)
	}
}

// exprOrType:
// 	expr
// |	nonExprType %prec _PreferToRightParen
func (p *parser) exprOrType() /*TODO return value */ Token {
more:
	var fix bool
	switch p.c {
	case token.IDENT, token.INT, token.FLOAT, token.IMAG, token.CHAR, token.STRING,
		token.NOT, token.AND, token.ADD, token.SUB, token.XOR, token.LPAREN:
		tok, _ := p.expr()
		return tok
	case token.ARROW:
		switch p.n() {
		case token.CHAN:
			p.n()
			p.typ()
		default:
			p.expr()
		}
	case token.MUL:
		p.n()
		goto more
	case token.CHAN, token.INTERFACE, token.MAP, token.STRUCT, token.LBRACK:
		p.otherType()
		switch p.c {
		case token.LBRACE, token.LPAREN:
			p.primaryExpr2(nil)
			p.expr2()
		}
	case token.FUNC:
		p.fnType()
		switch p.c {
		case tokenBODY:
			fix = true
			fallthrough
		case token.LBRACE:
			p.n()
			p.stmtList()
			p.loophack = fix
			p.must(token.RBRACE)
			p.pop()
			if p.c == token.LPAREN {
				p.primaryExpr2(nil)
				p.expr2()
			}
		default:
			p.pop()
		}
	default:
		p.syntaxError(p)
	}
	return Token{}
}

// exprOrTypeList:
// 	exprOrType
// |	exprOrTypeList ',' exprOrType
func (p *parser) exprOrTypeList() /*TODO return value */ (r []Token) {
	tok := p.exprOrType()
	if tok.Pos.IsValid() {
		r = []Token{tok}
	}
	for p.opt(token.COMMA) && p.not2(token.RPAREN, token.ELLIPSIS) {
		if tok = p.exprOrType(); r != nil && tok.Pos.IsValid() {
			r = append(r, tok)
		}
	}
	return r
}

// exprOpt:
// |	expr
func (p *parser) exprOpt() (isExprPresent bool) /*TODO return value */ {
	if p.c == token.COLON || p.c == token.RBRACK {
		return false
	}

	p.expr()
	return true
}

// primaryExpr:
// 	'(' exprOrType ')'
// |	IDENT genericArgsOpt %prec _NotParen
// |	convType '(' expr commaOpt ')'
// |	fnType lbrace stmtList '}'
// |	literal
// |	otherType lbrace bracedKeyValList '}'
// |	primaryExpr '(' ')'
// |	primaryExpr '(' exprOrTypeList "..." commaOpt ')'
// |	primaryExpr '(' exprOrTypeList commaOpt ')'
// |	primaryExpr '.' '(' "type" ')'
// |	primaryExpr '.' '(' exprOrType ')'
// |	primaryExpr '.' IDENT
// |	primaryExpr '[' expr ']'
// |	primaryExpr '[' exprOpt ':' exprOpt ':' exprOpt ']'
// |	primaryExpr '[' exprOpt ':' exprOpt ']'
// |	primaryExpr '{' bracedKeyValList '}'
func (p *parser) primaryExpr() /*TODO return value */ (rt Token, isLabelOrCompLitKey bool) {
	var fix bool
	var q *Scope
	switch p.c {
	case token.LPAREN:
		p.n()
		p.exprOrType()
		p.must(token.RPAREN)
	case token.IDENT:
		tok := p.tok()
		p.n()
		p.genericArgsOpt()
		if p.c == token.COLON {
			if p.xref != nil {
				p.xref[tok] = nil
			}
			return rt, true
		}

		rt = tok
		if p.xref != nil {
			if d := p.scope.Lookup(p.sourceFile.Package, p.sourceFile.Scope, tok); d != nil && d.Kind() == ImportDeclaration {
				q = d.ImportSpec().Package.Scope
			}
		}
	case token.FUNC:
		p.fnType()
		switch p.c {
		case tokenBODY:
			fix = true
			fallthrough
		case token.LBRACE:
			p.n()
			p.stmtList()
			p.loophack = fix
			p.must(token.RBRACE)
			p.pop()
		case token.LPAREN:
			p.pop()
			p.n()
			p.expr()
			p.opt(token.COMMA)
			p.must(token.RPAREN)
		default:
			p.pop()
			p.syntaxError(p)
		}
	case token.INT, token.FLOAT, token.IMAG, token.CHAR, token.STRING:
		p.n()
	case token.CHAN, token.INTERFACE, token.MAP, token.STRUCT, token.LBRACK:
		p.otherType()
		switch p.c {
		case token.LPAREN:
			p.n()
			p.expr()
			p.opt(token.COMMA)
			p.must(token.RPAREN)
		case tokenBODY:
			fix = true
			fallthrough
		case token.LBRACE:
			p.n()
			p.bracedKeyValList()
			p.loophack = fix
			p.must(token.RBRACE)
		default:
			p.syntaxError(p)
		}
	default:
		p.syntaxError(p)
	}
	if !p.primaryExpr2(q) {
		rt = Token{Pos: token.NoPos}
	}
	return rt, false
}

func (p *parser) primaryExpr2(q *Scope) /*TODO return value */ (empty bool) {
	for empty = true; ; q, empty = nil, false {
		switch p.c {
		case token.LPAREN:
			p.n()
			if p.opt(token.RPAREN) {
				break
			}

			p.exprOrTypeList()
			_ = p.opt(token.ELLIPSIS) && p.opt(token.COMMA) //TODOOK
			p.must(token.RPAREN)
		case token.PERIOD:
			switch p.n() {
			case token.IDENT:
				tok := p.tok()
				if p.xref != nil {
					p.xref[tok] = q
				}
				p.n()
			case token.LPAREN:
				p.n()
				if !p.opt(token.TYPE) {
					p.exprOrType()
				}
				p.must(token.RPAREN)
			default:
				p.syntaxError(p)
				p.n()
			}
		case token.LBRACK:
			p.n()
			if !p.exprOpt() && p.c == token.RBRACK {
				p.syntaxError(p)
				break
			}

			if p.opt(token.COLON) {
				p.exprOpt()
				if p.opt(token.COLON) {
					p.exprOpt()
				}
			}
			p.must(token.RBRACK)
		case token.LBRACE:
			p.n()
			p.bracedKeyValList()
			p.must(token.RBRACE)
		default:
			return empty
		}
	}
}

// unaryExpr:
// 	"<-" unaryExpr
// |	'!' unaryExpr
// |	'&' unaryExpr
// |	'*' unaryExpr
// |	'+' unaryExpr
// |	'-' unaryExpr
// |	'^' unaryExpr
// |	primaryExpr
func (p *parser) unaryExpr() /*TODO return value */ (rt Token, isLabel bool) {
	isLabel = true
	for {
		switch p.c {
		case token.ARROW, token.NOT, token.AND, token.MUL, token.ADD, token.SUB,
			token.XOR:
			isLabel = false
			p.n()
		default:
			rt, label := p.primaryExpr()
			if !isLabel { // Not the primaryExpr alone case.
				rt = Token{}
			}
			return rt, label && isLabel
		}
	}
}

// expr:
// 	expr "!=" expr
// |	expr "&&" expr
// |	expr "&^" expr
// |	expr "<-" expr
// |	expr "<<" expr
// |	expr "<=" expr
// |	expr "==" expr
// |	expr ">=" expr
// |	expr ">>" expr
// |	expr "||" expr
// |	expr '%' expr
// |	expr '&' expr
// |	expr '*' expr
// |	expr '+' expr
// |	expr '-' expr
// |	expr '/' expr
// |	expr '<' expr
// |	expr '>' expr
// |	expr '^' expr
// |	expr '|' expr
// |	unaryExpr
func (p *parser) expr() /*TODO return value */ (rt Token, isLabel bool) {
	if rt, isLabel = p.unaryExpr(); isLabel {
		return rt, true
	}

	if !p.expr2() {
		rt = Token{}
	}
	return rt, false
}

func (p *parser) expr2() (empty bool) {
	for empty = true; ; empty = false {
		switch p.c {
		case token.NEQ, token.LAND, token.AND_NOT, token.ARROW, token.SHL,
			token.LEQ, token.EQL, token.GEQ, token.SHR, token.LOR, token.REM,
			token.AND, token.MUL, token.ADD, token.SUB, token.QUO, token.LSS,
			token.GTR, token.XOR, token.OR:
			p.n()
			p.expr()
		default:
			return empty
		}
	}
}

// exprList:
// 	expr
// |	exprList ',' expr
func (p *parser) exprList() /*TODO return value */ (r []Token) {
	tok, _ := p.expr()
	if tok.Pos.IsValid() {
		r = []Token{tok}
	}
	for p.opt(token.COMMA) {
		if tok, _ = p.expr(); r != nil && tok.Pos.IsValid() {
			r = append(r, tok)
		}
	}
	return r
}

// constSpec:
// 	identList
// |	identList '=' exprList
// |	identList typ
// |	identList typ '=' exprList
func (p *parser) constSpec() {
	l := p.identList()

	defer func() {
		pos := token.NoPos
		if p.scope.Kind != PackageScope {
			pos = p.pos()
		}
		for _, v := range l {
			p.scope.declare(p, newConstDecl(v, pos))
		}
	}()

	switch p.c {
	case token.RPAREN, token.SEMICOLON:
		return
	case token.ASSIGN:
		p.n()
		p.exprList()
		return
	}

	p.typ()
	if p.opt(token.ASSIGN) {
		p.exprList()
	}
	if p.not2(token.SEMICOLON, token.RPAREN) {
		p.syntaxError(p)
		p.skip(token.SEMICOLON, token.RPAREN)
	}
}

// constSpecList:
// 	constSpec
// |	constSpecList ';' constSpec
func (p *parser) constSpecList() {
	for p.constSpec(); p.opt(token.SEMICOLON) && p.c != token.RPAREN; {
		p.constSpec()
	}
}

// fieldDecl:
// 	'*' embededName literalOpt
// |	identList typ literalOpt
// |	embededName literalOpt
func (p *parser) fieldDecl() {
	switch p.c {
	case token.IDENT:
		tok := p.tok()
		switch p.n() {
		case token.INT, token.FLOAT, token.IMAG, token.CHAR, token.STRING:
			if p.xref != nil {
				p.xref[tok] = nil
			}
			p.n()
			return
		case token.SEMICOLON, token.RBRACE:
			return
		case token.PERIOD:
			p.n()
			p.must(token.IDENT)
		case token.COMMA:
			if p.xref != nil {
				p.xref[tok] = nil
			}
			p.n()
			l := p.identList()
			if p.xref != nil {
				for _, v := range l {
					p.xref[v] = nil
				}
			}
			fallthrough
		default:
			if p.xref != nil {
				p.xref[tok] = nil
			}
			p.typ()
		}
	case token.MUL:
		if p.n() == token.LPAREN {
			p.syntaxError(p)
			p.skip(token.SEMICOLON, token.RBRACE)
			return
		}

		p.qualifiedIdent()
	default:
		p.syntaxError(p)
		p.skip(token.SEMICOLON, token.RBRACE)
		return
	}

	switch p.c {
	case token.INT, token.FLOAT, token.IMAG, token.CHAR, token.STRING:
		p.n()
	}
}

// fieldDeclList:
// 	fieldDecl
// |	fieldDeclList ';' fieldDecl
func (p *parser) fieldDeclList() {
	for p.fieldDecl(); p.opt(token.SEMICOLON) && p.c != token.RBRACE; {
		p.fieldDecl()
	}
}

// interfaceDecl:
// 	IDENT '(' paramTypeListCommaOptOpt ')' result
// |	embededName
func (p *parser) interfaceDecl() {
	p.must(token.IDENT)
	switch p.c {
	case token.LPAREN:
		s := newScope(BlockScope, nil)
		p.push(s)
		p.n()
		p.paramTypeListCommaOptOpt()
		p.must(token.RPAREN)
		p.result()
		p.pop()
		s.Parent = nil
	case token.PERIOD:
		p.n()
		p.must(token.IDENT)
	case token.SEMICOLON, token.RBRACE:
		// nop
	default:
		p.syntaxError(p)
		p.skip(token.SEMICOLON, token.RBRACE)
	}
}

// interfaceDeclList:
// 	interfaceDecl
// |	interfaceDeclList ';' interfaceDecl
func (p *parser) interfaceDeclList() {
	for p.interfaceDecl(); p.opt(token.SEMICOLON) && p.c != token.RBRACE; {
		p.interfaceDecl()
	}
}

// otherType:
// 	"chan" "<-" typ
// |	"chan" '(' typ ')'
// |	"chan" qualifiedIdent
// |	"chan" fnType
// |	"chan" otherType
// |	"chan" ptrType
// |	"interface" lbrace '}'
// |	"interface" lbrace interfaceDeclList semiOpt '}'
// |	"map" '[' typ ']' typ
// |	"struct" lbrace '}'
// |	"struct" lbrace fieldDeclList semiOpt '}'
// |	'[' "..." ']' typ
// |	'[' exprOpt ']' typ
func (p *parser) otherType() /*TODO return value */ {
	var fix bool
	switch p.c {
	case token.CHAN:
		switch p.n() {
		case token.ARROW:
			p.n()
			p.typ()
		case token.LPAREN:
			p.n()
			p.typ()
			p.must(token.RPAREN)
		case token.IDENT:
			p.qualifiedIdent()
		case token.FUNC:
			p.fnType()
			p.pop()
		case token.MUL:
			p.ptrType()
		default:
			p.otherType()
		}
	case token.INTERFACE:
		switch p.n() {
		case tokenBODY:
			fix = true
			fallthrough
		case token.LBRACE:
			if p.n() != token.RBRACE {
				p.interfaceDeclList()
			}
			p.loophack = fix
			p.must(token.RBRACE)
		default:
			p.syntaxError(p)
		}
	case token.MAP:
		p.n()
		p.must(token.LBRACK)
		p.typ()
		p.must(token.RBRACK)
		p.typ()
	case token.STRUCT:
		switch p.n() {
		case tokenBODY:
			fix = true
			fallthrough
		case token.LBRACE:
			if p.n() != token.RBRACE {
				p.fieldDeclList()
			}
			p.loophack = fix
			p.must(token.RBRACE)
		default:
			p.syntaxError(p)
		}
	case token.LBRACK:
		p.n()
		if !p.opt(token.ELLIPSIS) {
			p.exprOpt()
		}
		p.must(token.RBRACK)
		p.typ()
	default:
		p.syntaxError(p)
	}
}

// qualifiedIdent:
// 	IDENT %prec _NotParen
// |	IDENT '.' IDENT
func (p *parser) qualifiedIdent() (tok, tok2 Token) {
	tok, _ = p.mustTok(token.IDENT)
	if p.opt(token.PERIOD) {
		tok2, _ = p.mustTok(token.IDENT)
		if p.xref != nil {
			if d := p.scope.Lookup(p.sourceFile.Package, p.sourceFile.Scope, tok); d != nil && d.Kind() == ImportDeclaration && isExported(tok2.Val) {
				p.xref[tok2] = d.ImportSpec().Package.Scope
			}
		}
	}
	return tok, tok2
}

// ptrType:
// 	'*' typ
func (p *parser) ptrType() /*TODO return value */ {
	for p.opt(token.MUL) {
	}
	p.typ()
}

// fnType:
// 	"func" '(' paramTypeListCommaOptOpt ')' result
func (p *parser) fnType() /*TODO return value */ {
	p.push(newScope(BlockScope, nil))
	p.n() // "func"
	p.must(token.LPAREN)
	p.paramTypeListCommaOptOpt()
	p.must(token.RPAREN)
	p.result()
}

// rxChanType:
// 	"<-" "chan" typ
func (p *parser) rxChanType() /*TODO return value */ {
	p.n() // "<-"
	p.must(token.CHAN)
	p.typ()
}

// typeList:
// 	typ
// |	typeList ',' typ
func (p *parser) typeList() /*TODO return value */ {
	for p.typ(); p.opt(token.COMMA) && p.c != tokenGTGT; {
		p.typ()
	}
}

// genericArgsOpt:
// |	"«" typeList commaOpt "»"
func (p *parser) genericArgsOpt() /*TODO return value */ {
	if p.opt(tokenLTLT) {
		p.typeList()
		p.opt(token.COMMA)
		p.must(tokenGTGT)
	}
}

// typ:
// 	'(' typ ')'
// |	qualifiedIdent genericArgsOpt
// |	fnType
// |	otherType
// |	ptrType
// |	rxChanType
func (p *parser) typ() /*TODO return value */ (tok Token) {
	switch p.c {
	case token.LPAREN:
		p.n()
		p.typ()
		p.must(token.RPAREN)
	case token.IDENT:
		t1, t2 := p.qualifiedIdent()
		if !t2.Pos.IsValid() {
			tok = t1
		}
		p.genericArgsOpt()
	case token.FUNC:
		p.fnType()
		p.pop()
	case token.CHAN, token.INTERFACE, token.MAP, token.STRUCT, token.LBRACK:
		p.otherType()
	case token.MUL:
		p.ptrType()
	case token.ARROW:
		p.rxChanType()
	default:
		p.syntaxError(p)
	}
	return tok
}

//genericParamsOpt:
//|	"«" identList "»"
func (p *parser) genericParamsOpt() /*TODO return value */ {
	if p.opt(tokenLTLT) {
		p.identList()
		p.opt(token.COMMA)
		p.must(tokenGTGT)
	}
}

// typeSpec:
//	IDENT genericParamsOpt typ
// |	IDENT '=' typ
func (p *parser) typeSpec() /*TODO return value */ {
	if tok, ok := p.mustTok(token.IDENT); ok {
		pos := token.NoPos
		if p.scope.Kind != PackageScope {
			pos = p.pos()
		}
		p.scope.declare(p, newTypeDecl(tok, pos))
	}
	switch p.c {
	case token.ASSIGN:
		if goVersion >= "1.9" {
			p.n()
			break
		}

		fallthrough
	default:
		p.genericParamsOpt()
	}
	p.typ()
}

// typeSpecList:
// 	typeSpec
// |	typeSpecList ';' typeSpec
func (p *parser) typeSpecList() {
	for p.typeSpec(); p.opt(token.SEMICOLON) && p.c != token.RPAREN; {
		p.typeSpec()
	}
}

// varSpec:
// 	identList '=' exprList
// |	identList typ
// |	identList typ '=' exprList
func (p *parser) varSpec() {
	l := p.identList()

	defer func() {
		pos := token.NoPos
		if p.scope.Kind != PackageScope {
			pos = p.pos()
		}
		for _, v := range l {
			p.scope.declare(p, newVarDecl(v, pos))
		}
	}()

	switch p.c {
	case token.ASSIGN:
		p.n()
		p.exprList()
		return
	case token.PERIOD:
		p.syntaxError(p)
		p.skip(token.SEMICOLON, token.RPAREN)
		return
	}

	p.typ()
	if p.opt(token.ASSIGN) {
		p.exprList()
	}
}

// varSpecList:
// 	varSpec
// |	varSpecList ';' varSpec
func (p *parser) varSpecList() {
	for p.varSpec(); p.opt(token.SEMICOLON) && p.c != token.RPAREN; {
		p.varSpec()
	}
}

// commonDecl:
// 	"const" '(' ')'
// |	"const" '(' constSpec ';' constSpecList semiOpt ')'
// |	"const" '(' constSpec semiOpt ')'
// |	"const" constSpec
// |	"type" '(' ')'
// |	"type" '(' typeSpecList semiOpt ')'
// |	"type" typeSpec
// |	"var" '(' ')'
// |	"var" '(' varSpecList semiOpt ')'
// |	"var" varSpec
func (p *parser) commonDecl() {
	switch p.c {
	case token.CONST:
		p.n()
		switch {
		case p.opt(token.LPAREN):
			if !p.opt(token.RPAREN) {
				p.constSpecList()
				p.must(token.RPAREN)
			}
		default:
			p.constSpec()
		}
	case token.TYPE:
		p.n()
		switch {
		case p.opt(token.LPAREN):
			if !p.opt(token.RPAREN) {
				p.typeSpecList()
				p.must(token.RPAREN)
			}
		default:
			p.typeSpec()
		}
	case token.VAR:
		p.n()
		switch {
		case p.opt(token.LPAREN):
			if !p.opt(token.RPAREN) {
				p.varSpecList()
				p.must(token.RPAREN)
			}
		default:
			p.varSpec()
		}
	}
}

// paramType:
// 	IDENT dddType
// |	IDENT typ
// |	dddType
// |	typ
func (p *parser) paramType() /*TODO return value */ (tok Token, hasName bool) {
	switch p.c {
	case token.IDENT:
		tok = p.tok()
		switch p.n() {
		case token.RPAREN:
			// nop
		case token.COMMA:
			// nop
		case token.PERIOD:
			p.n()
			p.must(token.IDENT)
		case tokenLTLT:
			p.genericArgsOpt()
		case token.ELLIPSIS:
			p.n()
			p.typ()
		default:
			hasName = true
			p.typ()
		}
	case token.ELLIPSIS:
		p.n()
		p.typ()
	default:
		tok = p.typ()
	}
	return tok, hasName
}

// paramTypeList:
// 	paramType
// |	paramTypeList ',' paramType
func (p *parser) paramTypeList() /*TODO return value */ {
	var names []Token
	tok, hasNames := p.paramType()
	if tok.Pos.IsValid() {
		names = []Token{tok}
	}
	for p.opt(token.COMMA) && p.c != token.RPAREN {
		t, hasName := p.paramType()
		hasNames = hasNames || hasName
		if t.Pos.IsValid() {
			names = append(names, t)
		}
	}
	if hasNames {
		for _, v := range names {
			p.scope.declare(p, newVarDecl(v, v.Pos))
		}
	}
}

// paramTypeListCommaOptOpt:
// |	paramTypeList commaOpt
func (p *parser) paramTypeListCommaOptOpt() /*TODO return value */ {
	if p.c != token.RPAREN {
		p.paramTypeList()
	}
}

// result:
// 	%prec _NotParen
// |	'(' paramTypeListCommaOptOpt ')'
// |	qualifiedIdent genericArgsOpt
// |	fnType
// |	otherType
// |	ptrType
// |	rxChanType
func (p *parser) result() /*TODO return value */ {
	switch p.c {
	case token.LBRACE, token.RPAREN, token.SEMICOLON, token.COMMA, tokenBODY,
		token.RBRACE, token.COLON, token.STRING, token.ASSIGN:
		// nop
	case token.LPAREN:
		p.n()
		p.paramTypeListCommaOptOpt()
		p.must(token.RPAREN)
	case token.IDENT:
		p.qualifiedIdent()
		p.genericArgsOpt()
	case token.FUNC:
		p.fnType()
		p.pop()
	case token.CHAN, token.INTERFACE, token.MAP, token.STRUCT, token.LBRACK:
		p.otherType()
	case token.MUL:
		p.ptrType()
	case token.ARROW:
		p.rxChanType()
	default:
		p.syntaxError(p)
	}
}

func (p *parser) shortVarDecl1(tok Token, visibility token.Pos) {
	if !tok.Pos.IsValid() {
		return
	}

	if _, ok := p.scope.Bindings[tok.Val]; !ok {
		p.scope.declare(p, newVarDecl(tok, visibility))
		if p.xref != nil {
			p.xref[tok] = nil
		}
	}
}

func (p *parser) shortVarDecl(tok Token, l []Token, visibility token.Pos) {
	p.shortVarDecl1(tok, visibility)
	for _, v := range l {
		p.shortVarDecl1(v, visibility)
	}
}

// simpleStmt:
// 	expr
// |	expr "%=" expr
// |	expr "&=" expr
// |	expr "&^=" expr
// |	expr "*=" expr
// |	expr "++"
// |	expr "+=" expr
// |	expr "--"
// |	expr "-=" expr
// |	expr "/=" expr
// |	expr "<<=" expr
// |	expr ">>=" expr
// |	expr "^=" expr
// |	expr "|=" expr
// |	exprList ":=" exprList
// |	exprList '=' exprList
func (p *parser) simpleStmt(acceptRange bool) /*TODO return value */ (isLabel, isRange bool) {
	first := true
	var tok Token
	if tok, isLabel = p.expr(); isLabel {
		return true, false
	}

	var l []Token
more:
	switch pc := p.c; pc {
	case token.REM_ASSIGN, token.AND_ASSIGN, token.AND_NOT_ASSIGN, token.MUL_ASSIGN,
		token.ADD_ASSIGN, token.SUB_ASSIGN, token.QUO_ASSIGN, token.SHL_ASSIGN,
		token.SHR_ASSIGN, token.XOR_ASSIGN, token.OR_ASSIGN:
		p.n()
		p.expr()
	case token.INC, token.DEC:
		p.n()
		return false, false
	case token.COMMA:
		if !first {
			p.syntaxError(p)
			break
		}

		first = false
		p.n()
		l = p.exprList()
		goto more
	case token.DEFINE, token.ASSIGN:
		p.n()
		if acceptRange && p.opt(token.RANGE) {
			isRange = true
		}
		p.exprList()
		if pc == token.DEFINE {
			p.shortVarDecl(tok, l, p.pos())
		}
		return false, isRange
	}
	return false, false
}

// simpleStmtOpt:
// |	simpleStmt
func (p *parser) simpleStmtOpt(acceptRange bool) /*TODO return value */ (isRange bool) {
	if p.c == token.SEMICOLON || p.c == tokenBODY {
		return false
	}

	_, isRange = p.simpleStmt(acceptRange)
	return isRange
}

// ifHeader:
// 	simpleStmtOpt
// |	simpleStmtOpt ';' simpleStmtOpt
func (p *parser) ifHeader() /*TODO return value */ {
	p.simpleStmtOpt(false)
	if p.opt(token.SEMICOLON) {
		p.simpleStmtOpt(false)
	}
	if p.c == token.SEMICOLON {
		p.syntaxError(p)
		p.n()
	}
}

// loopBody:
// 	BODY stmtList '}'
func (p *parser) loopBody() /*TODO return value */ {
	p.must(tokenBODY)
	p.push(newScope(BlockScope, nil))
	p.stmtList()
	p.must(token.RBRACE)
	p.pop()
}

// elseIfList:
// |	elseIfList "else" "if" ifHeader loopBody
func (p *parser) elseIfList() /*TODO return value */ (isElse bool) {
	for p.opt(token.ELSE) {
		if p.opt(token.IF) {
			p.ifHeader()
			p.loopBody()
			continue
		}

		return true // Consumed "else", "if" does not follow.
	}
	return false
}

// compoundStmt:
// 	'{' stmtList '}'
func (p *parser) compoundStmt() /*TODO return value */ {
	switch p.c {
	case token.LBRACE:
		p.n()
		p.push(newScope(BlockScope, nil))
		p.stmtList()
		p.must(token.RBRACE)
		p.pop()
	case token.SEMICOLON:
		p.syntaxError(p)
	default:
		p.syntaxError(p)
		p.n()
	}
}

// caseBlockList:
// |	caseBlockList "case" exprOrTypeList ":=" expr ':' stmtList
// |	caseBlockList "case" exprOrTypeList ':' stmtList
// |	caseBlockList "case" exprOrTypeList '=' expr ':' stmtList
// |	caseBlockList "default" ':' stmtList
func (p *parser) caseBlockList() /*TODO return value */ {
	for {
		p.push(newScope(BlockScope, nil))
		switch p.c {
		case token.CASE:
			p.n()
			l := p.exprOrTypeList()
			def := p.c == token.DEFINE
			if def || p.c == token.ASSIGN {
				p.n()
				p.expr()
			}
			p.must(token.COLON)
			if def {
				p.shortVarDecl(Token{}, l, p.pos())
			}
		case token.DEFAULT:
			p.n()
			p.must(token.COLON)
			p.stmtList()
		case token.IF:
			p.syntaxError(p)
			p.skip(token.COLON)
		default:
			p.pop()
			if p.c != token.RBRACE {
				p.syntaxError(p)
				p.skip(token.RBRACE)
			}
			return
		}

		p.stmtList()
		p.pop()
	}
}

// stmt:
// |	"break" identOpt
// |	"continue" identOpt
// |	"defer" primaryExpr
// |	"fallthrough"
// |	"for" "range" expr loopBody
// |	"for" exprList ":=" "range" expr loopBody
// |	"for" exprList '=' "range" expr loopBody
// |	"for" simpleStmtOpt ';' simpleStmtOpt ';' simpleStmtOpt loopBody
// |	"for" simpleStmtOpt loopBody
// |	"go" primaryExpr
// |	"goto" IDENT
// |	"if" ifHeader loopBody elseIfList
// |	"if" ifHeader loopBody elseIfList "else" compoundStmt
// |	"return"
// |	"return" exprList
// |	"select" BODY caseBlockList '}'
// |	"switch" ifHeader BODY caseBlockList '}'
// |	IDENT ':' stmt
// |	commonDecl
// |	compoundStmt
// |	simpleStmt
func (p *parser) stmt() /*TODO return value */ {
more:
	switch p.c {
	case token.SEMICOLON, token.RBRACE, token.CASE, token.DEFAULT:
		// nop
	case token.BREAK, token.CONTINUE:
		p.n()
		p.opt(token.IDENT)
	case token.DEFER, token.GO:
		p.n()
		p.expr()
	case token.FALLTHROUGH:
		p.n()
	case token.FOR:
		p.push(newScope(BlockScope, nil))
		switch p.n() {
		case token.RANGE:
			p.n()
			p.expr()
		default:
			if p.simpleStmtOpt(true) { // range
				break
			}

			if p.opt(token.SEMICOLON) {
				p.simpleStmtOpt(false)
				p.must(token.SEMICOLON)
				p.simpleStmtOpt(false)
			}
		}
		if p.c == token.SEMICOLON {
			p.syntaxError(p)
			p.skip(tokenBODY)
		}
		p.loopBody()
		p.pop()
	case token.GOTO:
		p.n()
		p.must(token.IDENT)
	case token.IF:
		p.n()
		p.push(newScope(BlockScope, nil))
		p.ifHeader()
		p.loopBody()
		if p.elseIfList() {
			p.compoundStmt()
		}
		p.pop()
	case token.RETURN:
		p.n()
		if p.not2(token.SEMICOLON, token.RBRACE) {
			p.exprList()
			if p.not2(token.SEMICOLON, token.RBRACE) {
				p.syntaxError(p)
				p.skip(token.SEMICOLON, token.RBRACE)
			}
		}
	case token.SELECT:
		p.n()
		p.must(tokenBODY)
		p.push(newScope(BlockScope, nil))
		p.caseBlockList()
		p.must(token.RBRACE)
		p.pop()
	case token.SWITCH:
		p.n()
		p.push(newScope(BlockScope, nil))
		p.ifHeader()
		p.must(tokenBODY)
		p.push(newScope(BlockScope, nil))
		p.caseBlockList()
		p.must(token.RBRACE)
		p.pop()
		p.pop()
	case token.CONST, token.TYPE, token.VAR:
		p.commonDecl()
	case token.LBRACE:
		p.compoundStmt()
	default:
		if isLabel, _ := p.simpleStmt(false); isLabel && p.opt(token.COLON) {
			goto more
		}
	}
	if p.c == token.COLON {
		p.syntaxError(p)
		p.n()
	}
}

// stmtList:
// 	stmt
// |	stmtList ';' stmt
func (p *parser) stmtList() /*TODO return value */ {
	for p.stmt(); p.opt(token.SEMICOLON) && p.not2(token.RBRACE, token.CASE, token.DEFAULT); {
		p.stmt()
	}
}

// fnBody:
// |	'{' stmtList '}'
func (p *parser) fnBody() /*TODO return value */ {
	if p.opt(token.LBRACE) {
		p.stmtList()
		p.must(token.RBRACE)
	}
	p.pop()
}

// topLevelDeclList:
// |	topLevelDeclList "func" '(' paramTypeListCommaOptOpt ')' IDENT genericParamsOpt '(' paramTypeListCommaOptOpt ')' result fnBody ';'
// |	topLevelDeclList "func" IDENT genericParamsOpt '(' paramTypeListCommaOptOpt ')' result fnBody ';'
// |	topLevelDeclList commonDecl ';'
func (p *parser) topLevelDeclList() {
	for p.c != token.EOF {
		p.scope = p.sourceFile.Package.Scope
		switch p.c {
		case token.FUNC:
			p.push(newScope(BlockScope, nil))
			switch p.n() {
			case token.IDENT:
				switch tok := p.tok(); {
				case tok.Val == "init":
					p.sourceFile.InitFunctions = append(p.sourceFile.InitFunctions, newFuncDecl(p.tok(), token.NoPos))
				default:
					p.scope.Parent.declare(p, newFuncDecl(p.tok(), token.NoPos))
				}
				p.n()
				p.genericParamsOpt()
				switch p.c {
				case token.LPAREN:
					p.n()
				default:
					p.syntaxError(p)
				}
			case token.LPAREN:
				p.n()
				p.paramTypeListCommaOptOpt()
				p.must(token.RPAREN)
				p.must(token.IDENT)
				p.genericParamsOpt()
				p.must2(token.LPAREN)
			default:
				p.syntaxError(p)
			}

			p.paramTypeListCommaOptOpt()
			if p.c == token.SEMICOLON {
				p.syntaxError(p)
				p.skip(token.RPAREN)
			}
			p.must(token.RPAREN)
			p.result()
			p.fnBody()
		case token.CONST, token.TYPE, token.VAR:
			p.commonDecl()
		default:
			p.syntaxError(p)
		}
		p.must(token.SEMICOLON)
	}
}

// file:
// 	"package" IDENT ';' imports topLevelDeclList
func (p *parser) file() {
	if p.syntaxError == nil {
		p.syntaxError = func(*parser) { p.err(p.position(), "syntax error") }
	}
	p.n()
	if !p.must(token.PACKAGE) {
		return
	}

	tok, ok := p.mustTok(token.IDENT)
	if !ok {
		return
	}

	if p.xref != nil {
		p.xref[tok] = nil
	}
	if p.must(token.SEMICOLON) && p.sourceFile.build {
		pkg := p.sourceFile.Package
		switch nm := pkg.Name; {
		case nm == "":
			pkg.Name = tok.Val
			pkg.named = tok.Pos
		default:
			if nm != tok.Val {
				//dbg("", tok, pkg.Name, pkg.named)
				//TODO p.todo()
			}
		}
		p.imports()
		p.topLevelDeclList()
		if p.scope != nil && p.scope.Kind != PackageScope {
			panic("internal error")
		}
	}
}
