%{
// Copyright 2016 The GC Authors. All rights reserved.
// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.
%}

/* Non ASCII character classes */
eof			\x80
other			\x81

any_to_eol		[^\x80\n\r]*
big_u_value		\\U{hex_digit}{hex_digit}{hex_digit}{hex_digit}{hex_digit}{hex_digit}{hex_digit}{hex_digit}
byte_value		{octal_byte_value}|{hex_byte_value}
comment			"/*"([^*\x80]|\*+[^*/\x80])*\*+\/
decimal_digit		[0-9]
decimal_lit		[1-9]{decimal_digit}*
decimals		{decimal_digit}+
escaped_char		\\[abfnrtv'\x22\\]
exponent		[eE][+-]?{decimals}
float_lit		{decimals}"."{decimals}?{exponent}?|{decimals}{exponent}|"."{decimals}{exponent}?
hex_byte_value		\\x{hex_digit}{hex_digit}
hex_digit		[0-9a-fA-F]
hex_lit			0[xX]{hex_digit}+
identifier		{letter}({letter}|[0-9])*
imaginary_lit		({decimals}|{float_lit})"i"
int_lit			{decimal_lit}|{octal_lit}|{hex_lit}
interpreted_string_lit	\x22({str_unicode_value}|{byte_value})*\x22
letter			[a-zA-Z_]|{other}
little_u_value		\\u{hex_digit}{hex_digit}{hex_digit}{hex_digit}
octal_byte_value	\\{octal_digit}{octal_digit}{octal_digit}
octal_digit		[0-7]
octal_lit		0{octal_digit}*
raw_string_lit		`[^`\x80]*`
rune_lit		'({rune_unicode_value}?|{byte_value})'
rune_unicode_value	[^\x80\n\r\\]|{little_u_value}|{big_u_value}|{escaped_char}
str_unicode_value	[^\x22\x80\n\r\\]|{little_u_value}|{big_u_value}|{escaped_char}
string_lit		{raw_string_lit}|{interpreted_string_lit}

%%
 
[ \t\r]+
"//"{any_to_eol}[\n\r\x80]
"/*"([^*\x80]|\*+[^*/\x80])*\*+\/
"!="
"%="
"&&"
"&="
"&^"
"&^="
"*="
"++"
"+="
"--"
"-="
"..."
"/="
":="
"<-"
"<<"
"<<="
"<="
"=="
">="
">>"
">>="
"^="
"|="
"||"
"break"
"case"
"chan"
"const"
"continue"
"default"
"defer"
"else"
"fallthrough"
"for"
"func"
"go"
"goto"
"if"
"import"
"interface"
"map"
"package"
"range"
"return"
"select"
"struct"
"switch"
"type"
"var"
{float_lit}
{identifier}
{imaginary_lit}
{int_lit}
{rune_lit}
{string_lit}
