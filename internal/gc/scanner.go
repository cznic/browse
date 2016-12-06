// Copyright 2016 The GC Authors. All rights reserved.
// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.

// +build !go1.8

package gc

import (
	"bytes"
	"go/token"
	"unicode/utf8"
)

// Non ASCII character classes.
const (
	classEOF = iota + 0x80
	classNonASCII
	classLTLT
	classGTGT
	classBOM
	classNext
)

const (
	maxTokenToken = token.VAR
)

// Additional tokens.
const (
	tokenNL   = iota + maxTokenToken + 1
	tokenLTLT // «
	tokenGTGT // »
	tokenBODY
	tokenBOM
)

var (
	bom           = []byte("\ufeff")
	lineDirective = []byte("//line")
	nlLit         = []byte{'\n'}

	semiTriggerTokens = [...]bool{
		token.BREAK:       true,
		token.CHAR:        true,
		token.CONTINUE:    true,
		token.DEC:         true,
		token.FALLTHROUGH: true,
		token.FLOAT:       true,
		token.IDENT:       true,
		token.IMAG:        true,
		token.INC:         true,
		token.INT:         true,
		token.RBRACE:      true,
		token.RBRACK:      true,
		token.RETURN:      true,
		token.RPAREN:      true,
		token.STRING:      true,
		tokenGTGT:         true,
	}
)

// Lexer tokenizes source code.
type Lexer struct {
	CommentHandler func(pos Position, lit []byte)
	errHandler     func(pos Position, msg string, args ...interface{})
	errorCount     int // Number of errors encountered.
	file           *string
	lit            []byte
	prev           token.Token
	src            []byte

	column        int32
	commentColumn int32
	commentLine   int32
	commentOfs    int32
	line          int32
	off           int32 // Next byte offset.

	b byte // Current byte.
	c byte // Current class.
}

// NewLexer returns a newly created Lexer.
func NewLexer(src []byte) *Lexer {
	l := &Lexer{
		column: 0,
		line:   1,
		src:    src,
	}
	if bytes.HasPrefix(src, bom) {
		l.off = int32(len(bom))
		l.column = int32(len([]rune(string(bom))))
	}
	l.n()
	return l
}

func (l *Lexer) init(src []byte) *Lexer {
	l.commentOfs = -1
	l.errorCount = 0
	l.lit = nil
	l.off = 0
	l.prev = tokenNL
	l.src = src
	l.line = 1
	l.column = 0
	l.c = classNext
	l.file = nil
	if bytes.HasPrefix(src, bom) {
		l.off = 3
		l.column = 2
	}
	l.n()
	return l
}

func (l *Lexer) err(pos Position, msg string, args ...interface{}) {
	l.errorCount++
	if l.errHandler != nil {
		l.errHandler(pos, msg, args...)
	}
}

// Returns class.
func (l *Lexer) n() byte { // n == next
	if l.off == int32(len(l.src)) {
		if l.c != classEOF {
			l.column++
		}
		l.c = classEOF
		l.b = 0xff // Invalid UTF-8 byte.
		l.lit = nil
		return l.c
	}

	l.b = l.src[l.off]
	l.off++
	l.column++
	l.c = l.b
	if l.b > 0x7f {
		l.c = classNonASCII
		switch l.b {
		case 0xc2: // {"«","»"}[0]
			if l.off < int32(len(l.src)) {
				switch l.src[l.off] {
				case 0xab:
					l.c = classLTLT
				case 0xbb:
					l.c = classGTGT
				}
			}
		case 0xef: // BOM[0]
			if l.off+1 < int32(len(l.src)) && l.src[l.off] == 0xbb && l.src[l.off+1] == 0xbf {
				l.err(newPosition(l.file, l.line, l.column), "illegal BOM")
				l.c = classBOM
			}
		}
	} else if l.b == 0 {
		l.err(newPosition(l.file, l.line, l.column), "illegal character NUL")
	}
	return l.c
}

func (l *Lexer) octals(max int) (n int) {
	for max != 0 && l.c >= '0' && l.c <= '7' {
		l.n()
		n++
		max--
	}
	return n
}

func (l *Lexer) decimals() {
	for l.c >= '0' && l.c <= '9' {
		l.n()
	}
}

func (l *Lexer) exponent() token.Token {
	switch l.c {
	case 'e', 'E':
		switch l.n() {
		case '+', '-':
			l.n()
		}
		l.decimals()
	}
	switch l.c {
	case 'i':
		l.n()
		return token.IMAG
	}

	return token.FLOAT
}

func (l *Lexer) hexadecimals(max int) (n int) {
	for max != 0 && (l.c >= '0' && l.c <= '9' || l.c >= 'a' && l.c <= 'f' || l.c >= 'A' && l.c <= 'F') {
		l.n()
		n++
		max--
	}
	return n
}

func isIdentNext(c byte) bool {
	return c >= 'a' && c <= 'z' || c >= 'A' && c <= 'Z' || c == '_' || c >= '0' && c <= '9' || c == classNonASCII
}

func (l *Lexer) ident() token.Token {
	for l.c >= 'a' && l.c <= 'z' || l.c >= 'A' && l.c <= 'Z' || l.c == '_' || l.c >= '0' && l.c <= '9' || l.c == classNonASCII {
		l.n()
	}
	return token.IDENT
}

func (l *Lexer) skip() rune {
	if c := l.c; c < 0x80 {
		l.n()
		return rune(c)
	}

	if l.c == classEOF {
		return -1
	}

	r, sz := utf8.DecodeRune(l.src[l.off-1:])
	n := int32(sz) - 1
	l.off += n
	l.column += n
	l.n()
	return r
}

func (l *Lexer) stringEscFail() bool {
	switch l.c {
	case '\n':
		l.err(newPosition(l.file, l.line, l.column), "illegal character %#U in escape sequence", l.c)
	case '"':
		l.err(newPosition(l.file, l.line, l.column), "illegal character %#U in escape sequence", l.c)
		l.n()
		return true
	case '\\':
		l.err(newPosition(l.file, l.line, l.column), "illegal character %#U in escape sequence", l.c)
		fallthrough
	case classEOF:
		l.err(newPosition(l.file, l.line, l.column), "escape sequence not terminated")
	default:
		l.err(newPosition(l.file, l.line, l.column), "illegal character %#U in escape sequence", l.skip())
	}
	return false
}

func (l *Lexer) charEscFail() {
	switch l.c {
	case '\n':
		l.err(newPosition(l.file, l.line, l.column), "illegal character %#U in escape sequence", l.c)
	case '\\':
		l.err(newPosition(l.file, l.line, l.column), "illegal character %#U in escape sequence", l.c)
		l.n()
		fallthrough
	case classEOF:
		l.err(newPosition(l.file, l.line, l.column), "escape sequence not terminated")
	default:
		l.err(newPosition(l.file, l.line, l.column), "illegal character %#U in escape sequence", l.skip())
	}
}

// Scan returns the next token and its offset, line and column.
func (l *Lexer) Scan() (off, line, column int32, tok token.Token) {
skip:
	off, line, column, tok = l.scan0()
	if tok == token.COMMENT {
		if l.CommentHandler != nil {
			end := l.off
			if l.c != classEOF {
				end--
			}
			l.CommentHandler(newPosition(l.file, line, column), l.src[off:end])
		}
		if l.commentOfs < 0 {
			l.commentOfs = off
			l.commentLine = line
			l.commentColumn = column
		}
		goto skip
	}

	co := l.commentOfs
	l.commentOfs = -1
	if tok == tokenNL || tok == token.EOF {
		if p := int(l.prev); p >= 0 && p < len(semiTriggerTokens) && semiTriggerTokens[l.prev] {
			if co >= 0 {
				off = co
				line = l.commentLine
				column = l.commentColumn
			}
			l.prev = tok
			l.lit = nlLit
			if tok == token.EOF && co < 0 {
				off = l.off
			}
			return off, line, column, token.SEMICOLON
		}

		if tok == token.EOF {
			l.lit = nil
			return l.off, line, column, token.EOF
		}

		goto skip
	}

	if tok != token.ILLEGAL {
		l.prev = tok
	}
	end := l.off
	if l.c != classEOF {
		end--
	}
	l.lit = l.src[off:end]
	return off, line, column, tok
}

func (l *Lexer) scan0() (off, line, column int32, tok token.Token) {
skip:
	off = l.off - 1
	line = l.line
	column = l.column
	switch l.c {
	case '\t', '\r', ' ':
		l.n()
		goto skip
	case '\n':
		if l.off != int32(len(l.src)) {
			l.line++
			l.column = 0
		}
		l.n()
		return off, line, column, tokenNL
	case '!':
		if l.n() == '=' {
			l.n()
			return off, line, column, token.NEQ
		}

		return off, line, column, token.NOT
	case '"':
		l.n()
	more:
		switch l.c {
		case '\n', classEOF:
			l.err(newPosition(l.file, line, column), "string literal not terminated")
		case '"':
			l.n()
		case '\\':
			switch l.n() {
			case '\n':
				l.err(newPosition(l.file, line, column), "unknown escape sequence")
			case '0', '1', '2', '3', '4', '5', '6', '7':
				if l.octals(3) < 3 && l.stringEscFail() {
					return off, line, column, token.STRING
				}
			case 'a', 'b', 'f', 'n', 'r', 't', 'v', '\\', '"':
				l.n()
			case 'u':
				l.n()
				if l.hexadecimals(4) < 4 && l.stringEscFail() {
					return off, line, column, token.STRING
				}
			case 'U':
				l.n()
				if l.hexadecimals(8) < 8 && l.stringEscFail() {
					return off, line, column, token.STRING
				}
			case 'x':
				l.n()
				if l.hexadecimals(2) < 2 && l.c == classEOF {
					l.err(newPosition(l.file, line, column), "escape sequence not terminated")
				}
			case classEOF:
				l.err(newPosition(l.file, line, column), "escape sequence not terminated")
			default:
				l.err(newPosition(l.file, line, column), "unknown escape sequence")
				l.skip()
			}
			goto more
		default:
			l.skip()
			goto more
		}
		return off, line, column, token.STRING
	case '%':
		if l.n() == '=' {
			l.n()
			return off, line, column, token.REM_ASSIGN
		}

		return off, line, column, token.REM
	case '&':
		switch l.n() {
		case '^':
			if l.n() == '=' {
				l.n()
				return off, line, column, token.AND_NOT_ASSIGN
			}

			return off, line, column, token.AND_NOT
		case '=':
			l.n()
			return off, line, column, token.AND_ASSIGN
		case '&':
			l.n()
			return off, line, column, token.LAND
		}

		return off, line, column, token.AND
	case '\'':
		switch l.n() {
		case '\n', classEOF:
			l.err(newPosition(l.file, line, column), "rune literal not terminated")
		case '\'':
			l.err(newPosition(l.file, line, column), "illegal rune literal")
			l.n()
			return off, line, column, token.CHAR
		case '\\':
			switch l.n() {
			case '\n':
				l.err(newPosition(l.file, line, column), "unknown escape sequence")
				return off, line, column, token.CHAR
			case '0', '1', '2', '3', '4', '5', '6', '7':
				if l.octals(3) < 3 {
					l.charEscFail()
					return off, line, column, token.CHAR
				}
			case 'a', 'b', 'f', 'n', 'r', 't', 'v', '\\', '\'':
				l.n()
			case 'u':
				l.n()
				if l.hexadecimals(4) < 4 {
					l.charEscFail()
					return off, line, column, token.CHAR
				}
			case 'U':
				l.n()
				if l.hexadecimals(8) < 8 {
					l.charEscFail()
					return off, line, column, token.CHAR
				}
			case 'x':
				l.n()
				if l.hexadecimals(2) < 2 {
					l.charEscFail()
					return off, line, column, token.CHAR
				}
			case classEOF:
				l.err(newPosition(l.file, line, column), "escape sequence not terminated")
				return off, line, column, token.CHAR
			default:
				l.err(newPosition(l.file, line, column), "unknown escape sequence")
				l.skip()
				return off, line, column, token.CHAR
			}
		default:
			l.skip()
		}
		switch l.c {
		case '\n', classEOF:
			l.err(newPosition(l.file, line, column), "rune literal not terminated")
		case '\\':
			l.err(newPosition(l.file, line, column), "escape sequence not terminated")
			l.n()
		case '\'':
			l.n()
		default:
			l.err(newPosition(l.file, line, column), "rune literal not terminated")
			for l.n() != '\'' && l.c != classEOF && l.c != '\n' {
			}
		}
		return off, line, column, token.CHAR
	case '(':
		l.n()
		return off, line, column, token.LPAREN
	case ')':
		l.n()
		return off, line, column, token.RPAREN
	case '*':
		if l.n() == '=' {
			l.n()
			return off, line, column, token.MUL_ASSIGN
		}

		return off, line, column, token.MUL
	case '+':
		switch l.n() {
		case '=':
			l.n()
			return off, line, column, token.ADD_ASSIGN
		case '+':
			l.n()
			return off, line, column, token.INC
		}

		return off, line, column, token.ADD
	case ',':
		l.n()
		return off, line, column, token.COMMA
	case '-':
		switch l.n() {
		case '=':
			l.n()
			return off, line, column, token.SUB_ASSIGN
		case '-':
			l.n()
			return off, line, column, token.DEC
		}

		return off, line, column, token.SUB
	case '.':
		switch l.n() {
		case '0', '1', '2', '3', '4', '5', '6', '7', '8', '9':
			l.decimals()
			return off, line, column, l.exponent()
		case '.':
			switch l.n() {
			case '.':
				l.n()
				return off, line, column, token.ELLIPSIS
			default:
				l.off--
				return off, line, column, token.PERIOD
			}
		default:
			return off, line, column, token.PERIOD
		}
	case '/':
		switch l.n() {
		case '/':
			for l.n() != '\n' && l.c != classEOF {
			}
			return off, line, column, token.COMMENT
		case '*':
			var hasNL bool
			for l.n(); l.c != classEOF; l.n() {
				switch l.c {
				case '\n':
					hasNL = true
					if l.off != int32(len(l.src)) {
						l.line++
						l.column = 0
					}
				case '*':
				more2:
					switch l.n() {
					case '*':
						goto more2
					case '\n':
						hasNL = true
						if l.off != int32(len(l.src)) {
							l.line++
							l.column = 0
						}
					case '/':
						l.n()
						if hasNL {
							if l.CommentHandler != nil {
								end := l.off
								if l.c != classEOF {
									end--
								}
								l.CommentHandler(newPosition(l.file, line, column), l.src[off:end])
							}
							return off, line, column, tokenNL
						}

						return off, line, column, token.COMMENT
					}
				}
			}
			l.err(newPosition(l.file, line, column), "comment not terminated")
			return off, line, column, token.COMMENT
		case '=':
			l.n()
			return off, line, column, token.QUO_ASSIGN
		default:
			return off, line, column, token.QUO
		}
	case '0':
		n := l.octals(-1)
		switch l.c {
		case '.':
			l.n()
			l.decimals()
			return off, line, column, l.exponent()
		case '8', '9':
			l.decimals()
			switch l.c {
			case '.':
				l.n()
				l.decimals()
				return off, line, column, l.exponent()
			case 'e', 'E':
				return off, line, column, l.exponent()
			case 'i':
				l.n()
				return off, line, column, token.IMAG
			default:
				l.err(newPosition(l.file, line, column), "illegal octal number")
			}
		case 'e', 'E':
			return off, line, column, l.exponent()
		case 'i':
			l.n()
			return off, line, column, token.IMAG
		case 'x', 'X':
			if n != 1 {
				break
			}

			l.n()
			if l.hexadecimals(-1) == 0 {
				l.err(newPosition(l.file, line, column), "illegal hexadecimal number")
			}
		}

		return off, line, column, token.INT
	case '1', '2', '3', '4', '5', '6', '7', '8', '9':
		l.decimals()
		switch l.c {
		case '.':
			l.n()
			l.decimals()
			return off, line, column, l.exponent()
		case 'e', 'E':
			return off, line, column, l.exponent()
		case 'i':
			l.n()
			return off, line, column, token.IMAG
		}
		return off, line, column, token.INT
	case ':':
		if l.n() == '=' {
			l.n()
			return off, line, column, token.DEFINE
		}

		return off, line, column, token.COLON
	case ';':
		l.n()
		return off, line, column, token.SEMICOLON
	case '<':
		switch l.n() {
		case '<':
			if l.n() == '=' {
				l.n()
				return off, line, column, token.SHL_ASSIGN
			}

			return off, line, column, token.SHL
		case '=':
			l.n()
			return off, line, column, token.LEQ
		case '-':
			l.n()
			return off, line, column, token.ARROW
		}

		return off, line, column, token.LSS
	case '=':
		if l.n() == '=' {
			l.n()
			return off, line, column, token.EQL
		}

		return off, line, column, token.ASSIGN
	case '>':
		switch l.n() {
		case '>':
			if l.n() == '=' {
				l.n()
				return off, line, column, token.SHR_ASSIGN
			}

			return off, line, column, token.SHR
		case '=':
			l.n()
			return off, line, column, token.GEQ
		}

		return off, line, column, token.GTR
	case '[':
		l.n()
		return off, line, column, token.LBRACK
	case ']':
		l.n()
		return off, line, column, token.RBRACK
	case '^':
		if l.n() == '=' {
			l.n()
			return off, line, column, token.XOR_ASSIGN
		}

		return off, line, column, token.XOR
	case '`':
	more3:
		switch l.n() {
		case '`':
			l.n()
			return off, line, column, token.STRING
		case '\n':
			if l.off != int32(len(l.src)) {
				l.line++
				l.column = 0
			}
		case classEOF:
			l.err(newPosition(l.file, line, column), "raw string literal not terminated")
			return off, line, column, token.STRING
		}
		goto more3
	case 'b':
		if l.n() == 'r' && l.n() == 'e' && l.n() == 'a' && l.n() == 'k' && !isIdentNext(l.n()) {
			return off, line, column, token.BREAK
		}

		return off, line, column, l.ident()
	case 'c':
		switch l.n() {
		case 'a':
			if l.n() == 's' && l.n() == 'e' && !isIdentNext(l.n()) {
				return off, line, column, token.CASE
			}
		case 'h':
			if l.n() == 'a' && l.n() == 'n' && !isIdentNext(l.n()) {
				return off, line, column, token.CHAN
			}
		case 'o':
			if l.n() == 'n' {
				switch l.n() {
				case 's':
					if l.n() == 't' && !isIdentNext(l.n()) {
						return off, line, column, token.CONST
					}
				case 't':
					if l.n() == 'i' && l.n() == 'n' && l.n() == 'u' && l.n() == 'e' && !isIdentNext(l.n()) {
						return off, line, column, token.CONTINUE
					}
				}
			}
		}

		return off, line, column, l.ident()
	case 'd':
		if l.n() == 'e' && l.n() == 'f' {
			switch l.n() {
			case 'a':
				if l.n() == 'u' && l.n() == 'l' && l.n() == 't' && !isIdentNext(l.n()) {
					return off, line, column, token.DEFAULT
				}
			case 'e':
				if l.n() == 'r' && !isIdentNext(l.n()) {
					return off, line, column, token.DEFER
				}
			}
		}

		return off, line, column, l.ident()
	case 'e':
		if l.n() == 'l' && l.n() == 's' && l.n() == 'e' && !isIdentNext(l.n()) {
			return off, line, column, token.ELSE
		}

		return off, line, column, l.ident()
	case 'f':
		switch l.n() {
		case 'a':
			if l.n() == 'l' && l.n() == 'l' && l.n() == 't' && l.n() == 'h' && l.n() == 'r' && l.n() == 'o' && l.n() == 'u' && l.n() == 'g' && l.n() == 'h' && !isIdentNext(l.n()) {
				return off, line, column, token.FALLTHROUGH
			}
		case 'o':
			if l.n() == 'r' && !isIdentNext(l.n()) {
				return off, line, column, token.FOR
			}
		case 'u':
			if l.n() == 'n' && l.n() == 'c' && !isIdentNext(l.n()) {
				return off, line, column, token.FUNC
			}
		}

		return off, line, column, l.ident()
	case 'g':
		if l.n() == 'o' {
			if !isIdentNext(l.n()) {
				return off, line, column, token.GO
			}

			if l.c == 't' && l.n() == 'o' && !isIdentNext(l.n()) {
				return off, line, column, token.GOTO
			}
		}

		return off, line, column, l.ident()
	case 'i':
		switch l.n() {
		case 'f':
			if !isIdentNext(l.n()) {
				return off, line, column, token.IF
			}
		case 'm':
			if l.n() == 'p' && l.n() == 'o' && l.n() == 'r' && l.n() == 't' && !isIdentNext(l.n()) {
				return off, line, column, token.IMPORT
			}
		case 'n':
			if l.n() == 't' && l.n() == 'e' && l.n() == 'r' && l.n() == 'f' && l.n() == 'a' && l.n() == 'c' && l.n() == 'e' && !isIdentNext(l.n()) {
				return off, line, column, token.INTERFACE
			}
		}

		return off, line, column, l.ident()
	case 'm':
		if l.n() == 'a' && l.n() == 'p' && !isIdentNext(l.n()) {
			return off, line, column, token.MAP
		}

		return off, line, column, l.ident()
	case 'p':
		if l.n() == 'a' && l.n() == 'c' && l.n() == 'k' && l.n() == 'a' && l.n() == 'g' && l.n() == 'e' && !isIdentNext(l.n()) {
			return off, line, column, token.PACKAGE
		}

		return off, line, column, l.ident()
	case 'r':
		switch l.n() {
		case 'a':
			if l.n() == 'n' && l.n() == 'g' && l.n() == 'e' && !isIdentNext(l.n()) {
				return off, line, column, token.RANGE
			}
		case 'e':
			if l.n() == 't' && l.n() == 'u' && l.n() == 'r' && l.n() == 'n' && !isIdentNext(l.n()) {
				return off, line, column, token.RETURN
			}
		}

		return off, line, column, l.ident()
	case 's':
		switch l.n() {
		case 'e':
			if l.n() == 'l' && l.n() == 'e' && l.n() == 'c' && l.n() == 't' && !isIdentNext(l.n()) {
				return off, line, column, token.SELECT
			}
		case 't':
			if l.n() == 'r' && l.n() == 'u' && l.n() == 'c' && l.n() == 't' && !isIdentNext(l.n()) {
				return off, line, column, token.STRUCT
			}
		case 'w':
			if l.n() == 'i' && l.n() == 't' && l.n() == 'c' && l.n() == 'h' && !isIdentNext(l.n()) {
				return off, line, column, token.SWITCH
			}
		}

		return off, line, column, l.ident()
	case 't':
		if l.n() == 'y' && l.n() == 'p' && l.n() == 'e' && !isIdentNext(l.n()) {
			return off, line, column, token.TYPE
		}

		return off, line, column, l.ident()
	case 'v':
		if l.n() == 'a' && l.n() == 'r' && !isIdentNext(l.n()) {
			return off, line, column, token.VAR
		}

		return off, line, column, l.ident()
	case '{':
		l.n()
		return off, line, column, token.LBRACE
	case '|':
		switch l.n() {
		case '=':
			l.n()
			return off, line, column, token.OR_ASSIGN
		case '|':
			l.n()
			return off, line, column, token.LOR
		}

		return off, line, column, token.OR
	case '}':
		l.n()
		return off, line, column, token.RBRACE
	case classEOF:
		return off, line, column, token.EOF
	case classLTLT:
		l.skip()
		return off, line, column, tokenLTLT
	case classGTGT:
		l.skip()
		return off, line, column, tokenGTGT
	case classBOM:
		l.skip()
		return off, line, column, tokenBOM
	default:
		if l.c >= 'a' && l.c <= 'z' || l.c >= 'A' && l.c <= 'Z' || l.c == '_' || l.c == classNonASCII {
			l.n()
			for l.c >= 'a' && l.c <= 'z' || l.c >= 'A' && l.c <= 'Z' || l.c == '_' || l.c >= '0' && l.c <= '9' || l.c == classNonASCII {
				l.n()
			}
			return off, line, column, token.IDENT
		}

		switch {
		case l.b < ' ':
			l.err(newPosition(l.file, line, column), "illegal character %U", l.skip())
		default:
			l.err(newPosition(l.file, line, column), "illegal character %#U", l.skip())
		}
		return off, line, column, token.ILLEGAL
	}
}
