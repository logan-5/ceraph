%{
#include "ceraph_fwd.hpp"

#include "ast.hpp"
#include "codegen.hpp"
#include "operator.hpp"
#include "parser.hpp"

#include "llvm/Support/raw_ostream.h"

#include <iostream>

template <Operator::Unary Op, typename Operand>
ast::UnaryExpr make_unary(Operand&& operand);
template <Operator::Binary Op, typename Lhs, typename Rhs>
ast::BinaryExpr make_binary(Lhs&& lhs, Rhs&& rhs);

YY_DECL;

%}

%language "c++"

%define api.value.type variant
%define parse.assert
%define parse.error verbose

%type <Type::ID> TYPE_NAME
%type <ast::Node> expression term_expression product_expression unary_expression primary_expression 
%type <ast::FunctionProto> function_proto
%type <ast::Node> CONSTANT STRING_LITERAL
%type <std::string> IDENTIFIER
%type <std::vector<ast::FunctionProto::Arg>> maybe_param_list param_list
%type <ast::FunctionProto::Arg> param

%token IDENTIFIER CONSTANT STRING_LITERAL SIZEOF
%token PTR_OP INC_OP DEC_OP LEFT_OP RIGHT_OP LE_OP GE_OP EQ_OP NE_OP
%token AND_OP OR_OP MUL_ASSIGN DIV_ASSIGN MOD_ASSIGN ADD_ASSIGN
%token SUB_ASSIGN LEFT_ASSIGN RIGHT_ASSIGN AND_ASSIGN
%token XOR_ASSIGN OR_ASSIGN TYPE_NAME
%token LEX_ERROR

%token TYPEDEF EXTERN STATIC AUTO REGISTER
%token CHAR SHORT LONG SIGNED UNSIGNED CONST VOLATILE
%token STRUCT UNION ENUM ELLIPSIS

%token CASE DEFAULT IF ELSE SWITCH WHILE DO FOR GOTO CONTINUE BREAK RETURN

%%
program: lines
    | err program
    ;

err: error | LEX_ERROR { std::cerr << "lex error\n"; };

lines: | lines line;
line: expression ';' { 
    llvm::errs() << $1 << '\n'; 
    codegen::CodeGenInstance instance;
    if (auto* const code = $1.visit(codegen::Visitor{instance})) {
        code->print(llvm::errs());
        llvm::errs() << '\n';
        instance.verify(llvm::errs());
    } else {
        llvm::errs() << "no code generated\n";
    }
} 
| function_proto ';' { 
    codegen::CodeGenInstance instance;
    if (auto* const code = ast::Node{$1}.visit(codegen::Visitor{instance})) {
        code->print(llvm::errs());
        llvm::errs() << '\n';
        instance.verify(llvm::errs());
    } else {
        llvm::errs() << "no code generated\n";
    }
}
| ';';

primary_expression: 
    CONSTANT { $$ = std::move($1); }
	| STRING_LITERAL { $$ = std::move($1); }
	| '(' expression ')' { $$ = std::move($2); }
	;

unary_expression: primary_expression { $$ = std::move($1); }
    | '-' primary_expression { $$ = make_unary<Operator::Unary::Minus>($2); };

product_expression: unary_expression { $$ = std::move($1); }
    | product_expression '*' unary_expression { $$ = make_binary<Operator::Binary::Multiply>($1, $3); }
    | product_expression '/' unary_expression { $$ = make_binary<Operator::Binary::Divide>($1, $3); }
    | product_expression '%' unary_expression { $$ = make_binary<Operator::Binary::Modulo>($1, $3); }
    ;

term_expression: product_expression { $$ = std::move($1); }
    | term_expression '+' product_expression { $$ = make_binary<Operator::Binary::Plus>($1, $3); }
    | term_expression '-' product_expression { $$ = make_binary<Operator::Binary::Minus>($1, $3); }
    ;

expression: term_expression { $$ = std::move($1); };

function_proto: TYPE_NAME IDENTIFIER '(' maybe_param_list ')' { $$ = ast::FunctionProto{$1, std::move($2), std::move($4)}; };
maybe_param_list: param_list { $$ = std::move($1); } | { $$ = {}; };
param_list: param { $$ = {std::move($1)}; } 
    | param_list ',' param { $1.push_back(std::move($3)); $$ = std::move($1); };
param: TYPE_NAME IDENTIFIER { $$ = {$1, std::move($2)}; }
    | TYPE_NAME { $$ = {$1, std::nullopt}; };

%%

template <Operator::Unary Op, typename Operand>
ast::UnaryExpr make_unary(Operand&& operand) {
    return ast::UnaryExpr{Op, ast::make_nodeptr(std::move(operand))};
}
template <Operator::Binary Op, typename Lhs, typename Rhs>
ast::BinaryExpr make_binary(Lhs&& lhs, Rhs&& rhs) {
    return ast::BinaryExpr{Op, ast::make_nodeptr(std::move(lhs)), ast::make_nodeptr(std::move(rhs))};
}
