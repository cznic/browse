// This source file is a modification of [0].
//
// [0]: https://github.com/golang/go/blob/release-branch.go1.5/src/cmd/compile/internal/gc/go.y

// Copyright 2009 The Go Authors. All rights reserved.
// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.

%token
	ILLEGAL

	IDENT
	INT
	FLOAT
	IMAG
	CHAR
	STRING

	ADD_ASSIGN	"+="
	AND_ASSIGN	"&="
	AND_NOT		"&^"
	AND_NOT_ASSIGN	"&^="
	ARROW		"<-"
	DEC		"--"
	DEFINE		":="
	ELLIPSIS	"..."
	EQL		"=="
	GEQ		">="
	GTGT		"»"
	INC		"++"
	LAND		"&&"
	LEQ		"<="
	LOR		"||"
	LTLT		"«"
	MUL_ASSIGN	"*="
	NEQ		"!="
	OR_ASSIGN	"|="
	QUO_ASSIGN	"/="
	REM_ASSIGN	"%="
	SHL		"<<"
	SHL_ASSIGN	"<<="
	SHR		">>"
	SHR_ASSIGN	">>="
	SUB_ASSIGN	"-="
	XOR_ASSIGN	"^="
	 
	BREAK		"break"
	CASE		"case"
	CHAN		"chan"
	CONST		"const"
	CONTINUE	"continue"
	DEFAULT		"default"
	DEFER		"defer"
	ELSE		"else"
	FALLTHROUGH	"fallthrough"
	FOR		"for"
	FUNC		"func"
	GO		"go"
	GOTO		"goto"
	IF		"if"
	IMPORT		"import"
	INTERFACE	"interface"
	MAP		"map"
	PACKAGE		"package"
	RANGE		"range"
	RETURN		"return"
	SELECT		"select"
	STRUCT		"struct"
	SWITCH		"switch"
	TYPE		"type"
	VAR		"var"

	BODY 		"{"

%left	ARROW

%left	LOR
%left	LAND
%left	EQL NEQ LEQ GEQ '<' '>'
%left	'+' '-' '|' '^'
%left	'*' '/' '%' '&' SHL SHR AND_NOT

%left	_NotPackage
%left	PACKAGE

%left	_NotParen
%left	'('

%left	')'
%left	_PreferToRightParen

%%

file:
	"package" IDENT ';' imports topLevelDeclList

imports:
|	imports "import" '(' ')' ';'
|	imports "import" '(' importSpecList semiOpt ')' ';'
|	imports "import" importSpec ';'

importSpecList:
	importSpec
|	importSpecList ';' importSpec

importSpec:
	'.' STRING
|	IDENT STRING
|	STRING

topLevelDeclList:
|	topLevelDeclList "func" '(' paramTypeListCommaOptOpt ')' IDENT genericParamsOpt '(' paramTypeListCommaOptOpt ')' result fnBody ';'
|	topLevelDeclList "func" IDENT genericParamsOpt '(' paramTypeListCommaOptOpt ')' result fnBody ';'
|	topLevelDeclList commonDecl ';'

commonDecl:
	"const" '(' ')'
|	"const" '(' constSpec ';' constSpecList semiOpt ')'
|	"const" '(' constSpec semiOpt ')'
|	"const" constSpec
|	"type" '(' ')'
|	"type" '(' typeSpecList semiOpt ')'
|	"type" typeSpec
|	"var" '(' ')'
|	"var" '(' varSpecList semiOpt ')'
|	"var" varSpec

varSpec:
	identList '=' exprList
|	identList typ
|	identList typ '=' exprList

constSpec:
	identList
|	identList '=' exprList
|	identList typ
|	identList typ '=' exprList

genericArgsOpt:
|	"«" typeList commaOpt "»"

genericParamsOpt:
|	"«" identList commaOpt "»"

typeSpec:
	IDENT genericParamsOpt typ
|	IDENT '=' typ

simpleStmt:
	expr
|	expr "%=" expr
|	expr "&=" expr
|	expr "&^=" expr
|	expr "*=" expr
|	expr "++"
|	expr "+=" expr
|	expr "--"
|	expr "-=" expr
|	expr "/=" expr
|	expr "<<=" expr
|	expr ">>=" expr
|	expr "^=" expr
|	expr "|=" expr
|	exprList ":=" exprList
|	exprList '=' exprList

compoundStmt:
	'{' stmtList '}'

caseBlockList:
|	caseBlockList "case" exprOrTypeList ":=" expr ':' stmtList
|	caseBlockList "case" exprOrTypeList ':' stmtList
|	caseBlockList "case" exprOrTypeList '=' expr ':' stmtList
|	caseBlockList "default" ':' stmtList

loopBody:
	BODY stmtList '}'

ifHeader:
	simpleStmtOpt
|	simpleStmtOpt ';' simpleStmtOpt

elseIfList:
|	elseIfList "else" "if" ifHeader loopBody

expr:
	expr "!=" expr
|	expr "&&" expr
|	expr "&^" expr
|	expr "<-" expr
|	expr "<<" expr
|	expr "<=" expr
|	expr "==" expr
|	expr ">=" expr
|	expr ">>" expr
|	expr "||" expr
|	expr '%' expr
|	expr '&' expr
|	expr '*' expr
|	expr '+' expr
|	expr '-' expr
|	expr '/' expr
|	expr '<' expr
|	expr '>' expr
|	expr '^' expr
|	expr '|' expr
|	unaryExpr

unaryExpr:
	"<-" unaryExpr
|	'!' unaryExpr
|	'&' unaryExpr
|	'*' unaryExpr
|	'+' unaryExpr
|	'-' unaryExpr
|	'^' unaryExpr
|	primaryExpr

keyVal:
	compLitExpr
|	compLitExpr ':' compLitExpr

compLitExpr:
	'{' bracedKeyValList '}'
|	expr

primaryExpr:
	'(' exprOrType ')'
|	IDENT genericArgsOpt %prec _NotParen
|	convType '(' expr commaOpt ')'
|	fnType lbrace stmtList '}'
|	literal
|	otherType lbrace bracedKeyValList '}'
|	primaryExpr '(' ')'
|	primaryExpr '(' exprOrTypeList "..." commaOpt ')'
|	primaryExpr '(' exprOrTypeList commaOpt ')'
|	primaryExpr '.' '(' "type" ')'
|	primaryExpr '.' '(' typ ')'
|	primaryExpr '.' IDENT genericArgsOpt
|	primaryExpr '[' expr ']'
|	primaryExpr '[' exprOpt ':' exprOpt ':' exprOpt ']'
|	primaryExpr '[' exprOpt ':' exprOpt ']'
|	primaryExpr '{' bracedKeyValList '}'

exprOrType:
	expr
|	nonExprType %prec _PreferToRightParen

lbrace:
	BODY
|	'{'

identOpt:
|	IDENT

dddType:
	"..." typ

typ:
	'(' typ ')'
|	qualifiedIdent genericArgsOpt
|	fnType
|	otherType
|	ptrType
|	rxChanType

typeList:
	typ
|	typeList ',' typ

nonExprType:
	'*' nonExprType
|	fnType
|	otherType
|	rxChanType

convType:
	fnType
|	otherType

qualifiedIdent:
	IDENT %prec _NotParen
|	IDENT '.' IDENT

otherType:
	"chan" "<-" typ
|	"chan" '(' typ ')'
|	"chan" qualifiedIdent
|	"chan" fnType
|	"chan" otherType
|	"chan" ptrType
|	"interface" lbrace '}'
|	"interface" lbrace interfaceDeclList semiOpt '}'
|	"map" '[' typ ']' typ
|	"struct" lbrace '}'
|	"struct" lbrace fieldDeclList semiOpt '}'
|	'[' "..." ']' typ
|	'[' exprOpt ']' typ

ptrType:
	'*' typ

rxChanType:
	"<-" "chan" typ

fnType:
	"func" '(' paramTypeListCommaOptOpt ')' result

fnBody:
|	'{' stmtList '}'

result:
	%prec _NotParen
|	'(' paramTypeListCommaOptOpt ')'
|	qualifiedIdent genericArgsOpt
|	fnType
|	otherType
|	ptrType
|	rxChanType

varSpecList:
	varSpec
|	varSpecList ';' varSpec

constSpecList:
	constSpec
|	constSpecList ';' constSpec

typeSpecList:
	typeSpec
|	typeSpecList ';' typeSpec

fieldDeclList:
	fieldDecl
|	fieldDeclList ';' fieldDecl

interfaceDeclList:
	interfaceDecl
|	interfaceDeclList ';' interfaceDecl

fieldDecl:
	'*' embededName literalOpt
|	identList typ literalOpt
|	embededName literalOpt

embededName:
	IDENT
|	IDENT '.' IDENT

interfaceDecl:
	IDENT '(' paramTypeListCommaOptOpt ')' result
|	embededName

paramType:
	IDENT dddType
|	IDENT typ
|	dddType
|	typ

paramTypeList:
	paramType
|	paramTypeList ',' paramType

paramTypeListCommaOptOpt:
|	paramTypeList commaOpt

stmt:
|	"break" identOpt
|	"continue" identOpt
|	"defer" primaryExpr
|	"fallthrough"
|	"for" "range" expr loopBody
|	"for" exprList ":=" "range" expr loopBody
|	"for" exprList '=' "range" expr loopBody
|	"for" simpleStmtOpt ';' simpleStmtOpt ';' simpleStmtOpt loopBody
|	"for" simpleStmtOpt loopBody
|	"go" primaryExpr
|	"goto" IDENT
|	"if" ifHeader loopBody elseIfList
|	"if" ifHeader loopBody elseIfList "else" compoundStmt
|	"return"
|	"return" exprList
|	"select" BODY caseBlockList '}'
|	"switch" ifHeader BODY caseBlockList '}'
|	IDENT ':' stmt
|	commonDecl
|	compoundStmt
|	simpleStmt

stmtList:
	stmt
|	stmtList ';' stmt

identList:
	IDENT
|	identList ',' IDENT

exprList:
	expr
|	exprList ',' expr

exprOrTypeList:
	exprOrType
|	exprOrTypeList ',' exprOrType

keyValList:
	keyVal
|	keyValList ',' keyVal

bracedKeyValList:
|	keyValList commaOpt

semiOpt:
|	';'

commaOpt:
|	','

exprOpt:
|	expr

simpleStmtOpt:
|	simpleStmt

literalOpt:
|	literal

literal:
	CHAR
|	FLOAT
|	IMAG
|	INT
|	STRING
