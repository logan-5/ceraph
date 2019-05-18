%{
#include "ceraph_fwd.hpp"

#include "ast.hpp"
#include "operator.hpp"
#include "parser.hpp"

#include <iostream>
#include <optional>

using token = yy::parser::token;

yy::parser::token_type check_type();
void comment();
void count();

template <typename Int>
std::optional<Int> parse_int(const char* str);
template <typename Float>
std::optional<Float> parse_float(const char* str);

%}

D			[0-9]
L			[a-zA-Z_]
H			[a-fA-F0-9]
E			[Ee][+-]?{D}+
FS			(f|F)
LFS         (l|L)
IS			(u|U|l|L)*

%%
"/*"			{ comment(); }

"auto"			{ count(); return token::AUTO; }
"break"			{ count(); return token::BREAK; }
"case"			{ count(); return token::CASE; }
"char"			{ count(); return token::CHAR; }
"const"			{ count(); return token::CONST; }
"continue"		{ count(); return token::CONTINUE; }
"default"		{ count(); return token::DEFAULT; }
"do"			{ count(); return token::DO; }
"else"			{ count(); return token::ELSE; }
"enum"			{ count(); return token::ENUM; }
"extern"		{ count(); return token::EXTERN; }
"for"			{ count(); return token::FOR; }
"goto"			{ count(); return token::GOTO; }
"if"			{ count(); return token::IF; }
"long"			{ count(); return token::LONG; }
"register"		{ count(); return token::REGISTER; }
"return"		{ count(); return token::RETURN; }
"short"			{ count(); return token::SHORT; }
"signed"		{ count(); return token::SIGNED; }
"sizeof"		{ count(); return token::SIZEOF; }
"static"		{ count(); return token::STATIC; }
"struct"		{ count(); return token::STRUCT; }
"switch"		{ count(); return token::SWITCH; }
"typedef"		{ count(); return token::TYPEDEF; }
"union"			{ count(); return token::UNION; }
"unsigned"		{ count(); return token::UNSIGNED; }
"volatile"		{ count(); return token::VOLATILE; }
"while"			{ count(); return token::WHILE; }

{L}({L}|{D})*		{ count(); if (const auto type = Type::from_name(yytext)) { yylval->build(*type); return token::TYPE_NAME; } else { yylval->build(std::string{yytext}); return check_type(); } }

0[xX]{H}+{IS}?		{ count(); if (auto i = parse_int<int>(yytext)) { yylval->build<ast::Node>(ast::IntLiteral(*i)); return token::CONSTANT; } return token::LEX_ERROR; }
0{D}+{IS}?		{ count(); if (auto i = parse_int<int>(yytext)) { yylval->build<ast::Node>(ast::IntLiteral(*i)); return token::CONSTANT; } return token::LEX_ERROR; }
{D}+{IS}?		{ count(); if (auto i = parse_int<int>(yytext)) { yylval->build<ast::Node>(ast::IntLiteral(*i)); return token::CONSTANT; } return token::LEX_ERROR; }
L?'(\\.|[^\\'])+'	{ count(); return token::CONSTANT; }

{D}+{E}		{ count(); if (auto f = parse_float<double>(yytext)) { yylval->build<ast::Node>(ast::DoubleLiteral(*f)); return token::CONSTANT; } return token::LEX_ERROR; }
{D}*"."{D}+({E})?	{ count(); if (auto f = parse_float<double>(yytext)) { yylval->build<ast::Node>(ast::DoubleLiteral(*f)); return token::CONSTANT; } }
{D}+"."{D}*({E})?	{ count(); if (auto f = parse_float<double>(yytext)) { yylval->build<ast::Node>(ast::DoubleLiteral(*f)); return token::CONSTANT; } }
{D}+{E}{FS}		    { count(); if (auto f = parse_float<float>(yytext)) { yylval->build<ast::Node>(ast::FloatLiteral(*f)); return token::CONSTANT; } return token::LEX_ERROR; }
{D}*"."{D}+({E})?{FS}	{ count(); if (auto f = parse_float<float>(yytext)) { yylval->build<ast::Node>(ast::FloatLiteral(*f)); return token::CONSTANT; } return token::LEX_ERROR; }
{D}+"."{D}*({E})?{FS}	{ count(); if (auto f = parse_float<float>(yytext)) { yylval->build<ast::Node>(ast::FloatLiteral(*f)); return token::CONSTANT; } return token::LEX_ERROR; }
{D}+{E}{LFS}		    { count(); if (auto f = parse_float</*long*/ double>(yytext)) { yylval->build<ast::Node>(ast::/*Long*/DoubleLiteral(*f)); return token::CONSTANT; } return token::LEX_ERROR; }
{D}*"."{D}+({E})?{LFS}	{ count(); if (auto f = parse_float</*long*/ double>(yytext)) { yylval->build<ast::Node>(ast::/*Long*/DoubleLiteral(*f)); return token::CONSTANT; } return token::LEX_ERROR; }
{D}+"."{D}*({E})?{LFS}	{ count(); if (auto f = parse_float</*long*/ double>(yytext)) { yylval->build<ast::Node>(ast::/*Long*/DoubleLiteral(*f)); return token::CONSTANT; } return token::LEX_ERROR; }

L?\"(\\.|[^\\"])*\"	{ count(); yylval->build<ast::Node>(ast::StringLiteral(yytext)); return token::STRING_LITERAL; }

"..."			{ count(); return token::ELLIPSIS; }
">>="			{ count(); return token::RIGHT_ASSIGN; }
"<<="			{ count(); return token::LEFT_ASSIGN; }
"+="			{ count(); return token::ADD_ASSIGN; }
"-="			{ count(); return token::SUB_ASSIGN; }
"*="			{ count(); return token::MUL_ASSIGN; }
"/="			{ count(); return token::DIV_ASSIGN; }
"%="			{ count(); return token::MOD_ASSIGN; }
"&="			{ count(); return token::AND_ASSIGN; }
"^="			{ count(); return token::XOR_ASSIGN; }
"|="			{ count(); return token::OR_ASSIGN; }
">>"			{ count(); return token::RIGHT_OP; }
"<<"			{ count(); return token::LEFT_OP; }
"++"			{ count(); return token::INC_OP; }
"--"			{ count(); return token::DEC_OP; }
"->"			{ count(); return token::PTR_OP; }
"&&"			{ count(); return token::AND_OP; }
"||"			{ count(); return token::OR_OP; }
"<="			{ count(); return token::LE_OP; }
">="			{ count(); return token::GE_OP; }
"=="			{ count(); return token::EQ_OP; }
"!="			{ count(); return token::NE_OP; }
";"			{ count(); return ';'; }
("{"|"<%")		{ count(); return '{'; }
("}"|"%>")		{ count(); return '}'; }
","			{ count(); return ','; }
":"			{ count(); return ':'; }
"="			{ count(); return '='; }
"("			{ count(); return '('; }
")"			{ count(); return ')'; }
("["|"<:")		{ count(); return '['; }
("]"|":>")		{ count(); return ']'; }
"."			{ count(); return '.'; }
"&"			{ count(); return '&'; }
"!"			{ count(); return '!'; }
"~"			{ count(); return '~'; }
"-"			{ count(); return '-'; }
"+"			{ count(); return '+'; }
"*"			{ count(); return '*'; }
"/"			{ count(); return '/'; }
"%"			{ count(); return '%'; }
"<"			{ count(); return '<'; }
">"			{ count(); return '>'; }
"^"			{ count(); return '^'; }
"|"			{ count(); return '|'; }
"?"			{ count(); return '?'; }

[ \t\v\n\f]		{ count(); }
.			{ /* ignore bad characters */ }

%%

void comment() {
	char c, c1;

loop:
	while ((c = yyinput()) != '*' && c != 0)
		putchar(c);

	if ((c1 = yyinput()) != '/' && c != 0)
	{
		unput(c1);
		goto loop;
	}

	if (c != 0)
		putchar(c1);
}

void count() {}

yy::parser::token_type check_type() {
/*
* pseudo code --- this is what it should check
*
*	if (yytext == type_name)
*		return(TYPE_NAME);
*
*	return(IDENTIFIER);
*/

/*
*	it actually will only return IDENTIFIER
*/

	return token::IDENTIFIER;
}

template <typename Int>
std::optional<Int> parse_int(const char* str) {
    char* end;
    Int ret = static_cast<Int>(std::strtol(str, &end, 0));
    if (!*end) {
        return ret;
    }
    return std::nullopt;
}

namespace {
template<typename F> F to_float(const char* start, char** end);
template<>
float to_float<float>(const char* start, char** end) {
    return std::strtof(start, end);
}
template<>
double to_float<double>(const char* start, char** end) {
    return std::strtod(start, end);
}
template<>
long double to_float<long double>(const char* start, char** end) {
    return std::strtold(start, end);
}
}

template <typename Float>
std::optional<Float> parse_float(const char* str) {
    char* end;
    Float ret = to_float<Float>(str, &end);
    if (const char last = *end; 
        !last 
        || (std::is_same_v<std::remove_cv_t<Float>, float> && (last == 'f' || last == 'F')) 
        || (std::is_same_v<std::remove_cv_t<Float>, /*long*/ double> && (last == 'l' || last == 'L'))) { 
        return ret;
    }
    return std::nullopt;
}
