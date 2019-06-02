%{
#include "ceraph_fwd.hpp"

#include "ast.hpp"
#include "codegen.hpp"
#include "operator.hpp"
#include "sema.hpp"
#include "parser.hpp"

#include "llvm/Support/raw_ostream.h"

#include <iostream>

template <typename NodeT>
ast::NodePtr ptr(NodeT&& node) {
    return ast::make_nodeptr(std::move(node));
}

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
%parse-param {codegen::CodeGenInstance& codegen}
%parse-param {sema::GetType& typechecker}

%type <Type::ID> NONVOID_TYPE type_or_void
%type <ast::Node> expression term_expression product_expression unary_expression primary_expression postfix_expression
%type <ast::Node> equality_expression relational_expression flow_expression
%type <ast::Node> CONSTANT STRING_LITERAL
%type <std::string> IDENTIFIER

%type <ast::Node> statement 
%type <ast::Block::Stmts> statements block_statements
%type <ast::Node> expression_statement
%type <ast::NullStmt> null_statement
%type <ast::Block> block
%type <ast::Return> return_statement

%type <ast::Declaration> declaration
%type <ast::Assignment> assignment

%type <ast::FunctionProto> function_proto
%type <ast::FunctionProto::Arg> param
%type <ast::FunctionProto::ArgList> maybe_param_list param_list
%type <ast::FunctionDef> function_def;

%type <ast::FunctionCall::Arg> call_arg;
%type <ast::FunctionCall::ArgList> maybe_call_arg_list call_arg_list;
%type <ast::FunctionCall> function_call_expression;

%type <ast::Node> if_else;

%type <ast::CrappyForLoop> crappy_for_loop;

%token IDENTIFIER CONSTANT STRING_LITERAL SIZEOF
%token PTR_OP INC_OP DEC_OP LEFT_OP RIGHT_OP LE_OP GE_OP EQ_OP NE_OP
%token AND_OP OR_OP MUL_ASSIGN DIV_ASSIGN MOD_ASSIGN ADD_ASSIGN
%token SUB_ASSIGN LEFT_ASSIGN RIGHT_ASSIGN AND_ASSIGN
%token XOR_ASSIGN OR_ASSIGN NONVOID_TYPE
%token LEX_ERROR

%token LET
%token TYPEDEF EXTERN STATIC AUTO REGISTER
%token CHAR SHORT LONG SIGNED UNSIGNED CONST VOLATILE
%token VOID
%token STRUCT UNION ENUM ELLIPSIS

%token CASE DEFAULT SWITCH WHILE DO FOR GOTO CONTINUE BREAK RETURN

%token IF
%right ')'
%right ELSE

%%
program: lines
    | err program
    ;

err: error | LEX_ERROR { std::cerr << "lex error\n"; };

lines: | lines line;
line: function_proto ';' { 
    if (auto typeE = typechecker($1)) {
        // llvm::errs() << "function block result: " << to_string(*typeE) << '\n';
    } else {
        llvm::errs() << "type error: " << typeE.takeError() << '\n';
    }
    if (auto codeE = ast::Node{$1}.visit(codegen::Visitor{codegen})) {
        const auto& code = *codeE;
        code->print(llvm::outs());
        llvm::errs() << '\n';
        assert(!codegen.verify(llvm::errs()));
    } else {
        llvm::errs() << "error: " << codeE.takeError() << '\n';
        llvm::errs() << "no code generated\n";
    }
}
| function_def { 
    if (auto typeE = typechecker($1)) {
        // llvm::errs() << "function block result: " << to_string(*typeE) << '\n';
    } else {
        llvm::errs() << "type error: " << typeE.takeError() << '\n';
    }
    if (auto codeE = ast::Node{$1}.visit(codegen::Visitor{codegen})) {
        const auto& code = *codeE;
        code->print(llvm::outs());
        llvm::errs() << '\n';
        assert(!codegen.verify(llvm::errs()));
    } else {
        llvm::errs() << "error: " << codeE.takeError() << '\n';
        llvm::errs() << "no code generated\n";
    }
};

statement
    : expression_statement { $$ = std::move($1); }
    | null_statement { $$ = std::move($1); }
    | declaration { $$ = std::move($1); }
    | return_statement { $$ = std::move($1); }
    ;

statements: statement { $$ = {ptr($1)}; }
    | statements statement { $1.push_back(ptr($2)); $$ = std::move($1); }
    ;

block_statements: statements { $1.push_back(ptr(ast::NullStmt{})); $$ = std::move($1); }
    | statements expression { $1.push_back(ptr($2)); $$ = std::move($1); }
    | expression { $$ = {ptr($1)}; }
    | { $$ = {ptr(ast::NullStmt{})}; }
    ;

block: '{' block_statements '}' { $$ = ast::Block{std::move($2)}; };

expression_statement: expression ';' { $$ = std::move($1); };
null_statement: ';' { $$ = ast::NullStmt{}; };

declaration: NONVOID_TYPE IDENTIFIER '=' expression ';' { $$ = ast::Declaration{$1, std::move($2), ptr($4)}; }
    | LET IDENTIFIER '=' expression ';' { $$ = ast::Declaration{std::nullopt, std::move($2), ptr($4)}; }
    ;
assignment: IDENTIFIER '=' expression { $$ = ast::Assignment{std::move($1), ptr($3)}; };

return_statement: RETURN ';' { $$ = ast::Return{}; }
    | RETURN expression ';' { $$ = ast::Return{ptr($2)}; }
    ;

primary_expression: 
    IDENTIFIER { $$ = ast::Identifier{std::move($1)}; }
    | CONSTANT { $$ = std::move($1); }
	| STRING_LITERAL { $$ = std::move($1); }
	| '(' expression ')' { $$ = std::move($2); }
	;

postfix_expression: primary_expression { $$ = std::move($1); }
    | function_call_expression { $$ = std::move($1); }
    ;

unary_expression: postfix_expression { $$ = std::move($1); }
    | '-' postfix_expression { $$ = make_unary<Operator::Unary::Minus>($2); }
    ;

product_expression: unary_expression { $$ = std::move($1); }
    | product_expression '*' unary_expression { $$ = make_binary<Operator::Binary::Multiply>($1, $3); }
    | product_expression '/' unary_expression { $$ = make_binary<Operator::Binary::Divide>($1, $3); }
    | product_expression '%' unary_expression { $$ = make_binary<Operator::Binary::Modulo>($1, $3); }
    ;

term_expression: product_expression { $$ = std::move($1); }
    | term_expression '+' product_expression { $$ = make_binary<Operator::Binary::Plus>($1, $3); }
    | term_expression '-' product_expression { $$ = make_binary<Operator::Binary::Minus>($1, $3); }
    ;

relational_expression: term_expression { $$ = std::move($1); }
    | relational_expression '<' term_expression { $$ = make_binary<Operator::Binary::Less>($1, $3); }
    ;

equality_expression: relational_expression { $$ = std::move($1); }
    | equality_expression EQ_OP relational_expression { $$ = make_binary<Operator::Binary::Equality>($1, $3); }
    ;

flow_expression: equality_expression { $$ = std::move($1); }
    | if_else { $$ = std::move($1); }
    | crappy_for_loop { $$ = std::move($1); }
    ;

expression
    : flow_expression { $$ = std::move($1); }
    | assignment { $$ = std::move($1); }
    ;

function_proto: type_or_void IDENTIFIER '(' maybe_param_list ')' { $$ = ast::FunctionProto{$1, std::move($2), std::move($4)}; };
maybe_param_list: param_list { $$ = std::move($1); } | empty_param_list { $$ = {}; };
empty_param_list: | VOID;
param_list: param { $$ = {std::move($1)}; } 
    | param_list ',' param { $1.push_back(std::move($3)); $$ = std::move($1); }
    ;
param: NONVOID_TYPE IDENTIFIER { $$ = {$1, std::move($2)}; }
    | NONVOID_TYPE { $$ = {$1, std::nullopt}; }
    ;

function_def: function_proto block[body] { $$ = ast::FunctionDef{std::move($1), ptr($body)}; };

function_call_expression: IDENTIFIER '(' maybe_call_arg_list ')' { $$ = ast::FunctionCall{std::move($1), std::move($3)}; };
maybe_call_arg_list: call_arg_list { $$ = std::move($1); } | empty_call_arg_list { $$ = {}; };
empty_call_arg_list: | VOID;
call_arg_list: call_arg { $$ = {std::move($1)}; } 
    | call_arg_list ',' call_arg { $1.push_back(std::move($3)); $$ = std::move($1); }
    ;
call_arg: expression { $$ = ptr($1); };

type_or_void: NONVOID_TYPE { $$ = $1; } | VOID { $$ = Type::ID::Void; };

if_else: IF expression[cond] block[then] { $$ = ast::IfElse{ptr($cond), ptr($then)}; }
    | IF expression[cond] block[then] ELSE block[else_] { $$ = ast::IfElse{ptr($cond), ptr($then), ptr($else_)}; }
    ;

crappy_for_loop: 
    FOR IDENTIFIER[ind] '=' expression[init] ',' expression[cond] ',' expression[incr] ':' expression[body] { $$ = ast::CrappyForLoop{std::move($ind), ptr($init), ptr($cond), ptr($incr), ptr($body)}; } ;

%%

template <Operator::Unary Op, typename Operand>
ast::UnaryExpr make_unary(Operand&& operand) {
    return ast::UnaryExpr{Op, ptr(operand)};
}
template <Operator::Binary Op, typename Lhs, typename Rhs>
ast::BinaryExpr make_binary(Lhs&& lhs, Rhs&& rhs) {
    return ast::BinaryExpr{Op, ptr(lhs), ptr(rhs)};
}
