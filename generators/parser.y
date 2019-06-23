%{
#include "ceraph_fwd.hpp"

#include "ast.hpp"
#include "codegen.hpp"
#include "operator.hpp"
#include "sema.hpp"
#include "parser_types.hpp"

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

bool field_already_exists(
    const ast::StructDef::Fields& fields, 
    const ast::StructDef::Field& theField);

template <typename T>
std::vector<std::remove_reference_t<T>> vec(T&& t);
template <typename W, typename T>
ast::VectorWrapper<std::remove_reference_t<T>, W> vec(T&& t);

YY_DECL;

%}

%language "c++"

%define api.value.type variant
%define parse.assert
%define parse.error verbose
%parse-param {codegen::CodeGenInstance& codegen}
%parse-param {sema::GetType& typechecker}
%lex-param {const LexerContext& codegen}

%type <Type::ID> BUILTIN_NONVOID_TYPE USER_DEFINED_TYPE
%type <Type::CompoundType> nonvoid_type type_or_void
%type <ast::Node> expression term_expression product_expression unary_expression primary_expression postfix_expression
%type <ast::Node> equality_expression relational_expression and_expression or_expression flow_expression
%type <ast::IntLiteral> INTEGER
%type <ast::Node> REAL CONSTANT STRING_LITERAL NULL_LITERAL
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

%type <ast::While> while_loop;

%type <ast::StructDef> struct_def;
%type <ast::StructDef::Fields> struct_fields;
%type <ast::StructDef::Field> struct_field;

%type <ast::StructValue> struct_value;
%type <ast::StructMemberAccess> struct_member_access;

%type <ast::ArrayLiteral> array_literal;
%type <ast::ArrayLiteral::Elems> array_literal_elems;

%type <bool> deref_operator;

%token IDENTIFIER INTEGER REAL CONSTANT STRING_LITERAL SIZEOF NULL_LITERAL
%token ARROW INC_OP DEC_OP LEFT_OP RIGHT_OP LE_OP GE_OP EQ_OP NE_OP
%token LOGICAL_AND LOGICAL_OR MUL_ASSIGN DIV_ASSIGN MOD_ASSIGN ADD_ASSIGN
%token SUB_ASSIGN LEFT_ASSIGN RIGHT_ASSIGN AND_ASSIGN
%token XOR_ASSIGN OR_ASSIGN BUILTIN_NONVOID_TYPE USER_DEFINED_TYPE
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
        (void)code;
        // code->print(llvm::outs());
        // llvm::errs() << '\n';
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
        (void)code;
        // code->print(llvm::outs());
        // llvm::errs() << '\n';
        assert(!codegen.verify(llvm::errs()));
    } else {
        llvm::errs() << "error: " << codeE.takeError() << '\n';
        llvm::errs() << "no code generated\n";
    }
}
| struct_def {
    if (codegen::Visitor{codegen}($1)) {
        llvm::errs() << "error";
    }
    assert(!codegen.verify(llvm::errs()));
    // codegen.dump(llvm::errs());
};

statement
    : expression_statement { $$ = std::move($1); }
    | null_statement { $$ = std::move($1); }
    | declaration { $$ = std::move($1); }
    | return_statement { $$ = std::move($1); }
    ;

statements: statement { $$ = vec<ast::Block>(ptr($1)); }
    | statements statement { $1.push_back(ptr($2)); $$ = std::move($1); }
    ;

block_statements: statements { $1.push_back(ptr(ast::NullStmt{})); $$ = std::move($1); }
    | statements expression { $1.push_back(ptr($2)); $$ = std::move($1); }
    | expression { $$ = vec<ast::Block>(ptr($1)); }
    | { $$ = vec<ast::Block>(ptr(ast::NullStmt{})); }
    ;

block: '{' block_statements '}' { $$ = ast::Block{std::move($2)}; };

expression_statement: expression ';' { $$ = std::move($1); };
null_statement: ';' { $$ = ast::NullStmt{}; };

declaration: nonvoid_type IDENTIFIER '=' expression ';' { $$ = ast::Declaration{std::move($1), std::move($2), ptr($4)}; }
    | LET IDENTIFIER '=' expression ';' { $$ = ast::Declaration{std::nullopt, std::move($2), ptr($4)}; }
    ;
assignment: flow_expression '=' expression { $$ = ast::Assignment{ptr($1), ptr($3)}; };

return_statement: RETURN ';' { $$ = ast::Return{}; }
    | RETURN expression ';' { $$ = ast::Return{ptr($2)}; }
    ;

primary_expression: 
    IDENTIFIER { $$ = ast::Identifier{std::move($1)}; }
    | INTEGER { $$ = std::move($1); }
    | REAL { $$ = std::move($1); }
    | CONSTANT { $$ = std::move($1); }
	| STRING_LITERAL { $$ = std::move($1); }
    | NULL_LITERAL { $$ = std::move($1); }
    | struct_value { $$ = std::move($1); }
    | array_literal { $$ = std::move($1); }
	| '(' expression ')' { $$ = std::move($2); }
	;

postfix_expression: primary_expression { $$ = std::move($1); }
    | function_call_expression { $$ = std::move($1); }
    | struct_member_access { $$ = std::move($1); }
    | postfix_expression '[' expression ']' { $$ = ast::Subscript{ptr($1), ptr($3)}; }
    ;

unary_expression: postfix_expression { $$ = std::move($1); }
    | '-' postfix_expression { $$ = make_unary<Operator::Unary::Minus>($2); }
    | '<' nonvoid_type[type] '>' unary_expression[expr] { $$ = ast::ExplicitCast{ptr($expr), std::move($type)}; }
    | '&' unary_expression { $$ = ast::AddressOf{ptr($2)}; }
    | '*' unary_expression { $$ = ast::Dereference{ptr($2)}; }
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

and_expression: equality_expression { $$ = std::move($1); }
    | and_expression LOGICAL_AND equality_expression { $$ = ast::LogicalAnd{ptr($1), ptr($3)}; }
    ;

or_expression: and_expression { $$ = std::move($1); }
    | or_expression LOGICAL_OR and_expression { $$ = ast::LogicalOr{ptr($1), ptr($3)}; }
    ;

flow_expression: or_expression { $$ = std::move($1); }
    | if_else { $$ = std::move($1); }
    | while_loop { $$ = std::move($1); }
    ;

expression
    : flow_expression { $$ = std::move($1); }
    | assignment { $$ = std::move($1); }
    ;

function_proto: type_or_void IDENTIFIER '(' maybe_param_list ')' { $$ = ast::FunctionProto{$1, std::move($2), std::move($4)}; };
maybe_param_list: param_list { $$ = std::move($1); } | empty_param_list { $$ = {}; };
empty_param_list: | VOID;
param_list: param { $$ = vec($1); } 
    | param_list ',' param { $1.push_back(std::move($3)); $$ = std::move($1); }
    ;
param: nonvoid_type IDENTIFIER { $$ = {std::move($1), std::move($2)}; }
    | nonvoid_type { $$ = {std::move($1), std::nullopt}; }
    ;

function_def: function_proto block[body] { $$ = ast::FunctionDef{std::move($1), ptr($body)}; };

function_call_expression: IDENTIFIER '(' maybe_call_arg_list ')' { $$ = ast::FunctionCall{std::move($1), std::move($3)}; };
maybe_call_arg_list: call_arg_list { $$ = std::move($1); } | empty_call_arg_list { $$ = {}; };
empty_call_arg_list: | VOID;
call_arg_list: call_arg { $$ = vec($1); } 
    | call_arg_list ',' call_arg { $1.push_back(std::move($3)); $$ = std::move($1); }
    ;
call_arg: expression { $$ = ptr($1); };

type_or_void: nonvoid_type { $$ = std::move($1); } | VOID { $$ = Type::ID::Void; };
nonvoid_type: BUILTIN_NONVOID_TYPE { $$ = $1; } 
    | USER_DEFINED_TYPE { $$ = $1; }
    | nonvoid_type '*' { $$ = Type::Pointer{std::move($1)}; }
    | '[' nonvoid_type[type] ':' INTEGER[size] ']' { $$ = Type::Array{std::move($type), static_cast<Type::Array::SizeType>($size.rep)}; }
    ;

if_else: IF expression[cond] block[then] { $$ = ast::IfElse{ptr($cond), ptr($then)}; }
    | IF expression[cond] block[then] ELSE block[else_] { $$ = ast::IfElse{ptr($cond), ptr($then), ptr($else_)}; }
    ;

while_loop: WHILE expression[cond] block[body] { $$ = ast::While{nullptr, ptr($cond), ptr($body)}; }
    | WHILE statement[init] expression[cond] block[body] { $$ = ast::While{ptr($init), ptr($cond), ptr($body)}; }
    ;

struct_def: STRUCT IDENTIFIER[name] '{' struct_fields[fields] '}' { $$ = ast::StructDef{std::move($name), std::move($fields)}; }
    | STRUCT IDENTIFIER[name] '{' '}' { $$ = ast::StructDef{std::move($name), {}}; }
    ;

struct_fields: struct_field { $$ = vec(std::move($1)); }
    | struct_fields struct_field { 
        if (field_already_exists($1, $2)) { 
            llvm::errs() << "duplicate struct member '" << $2.name << "'"; YYERROR;
        }
        $1.push_back(std::move($2));
        $$ = std::move($1);
    }
    ;

struct_field: nonvoid_type IDENTIFIER ';' { $$ = ast::StructDef::Field{std::move($2), std::move($1)}; };

struct_value: USER_DEFINED_TYPE '.' '{' '}' { $$ = ast::StructValue{$1}; };

struct_member_access: postfix_expression deref_operator[deref] IDENTIFIER { $$ = ast::StructMemberAccess{ptr($1), std::move($3), $deref}; };
deref_operator: '.' { $$ = false; } | ARROW { $$ = true; };

maybe_comma: ',' | ;

array_literal: '[' array_literal_elems[elems] maybe_comma ']' { $$ = ast::ArrayLiteral{std::move($elems)}; };

array_literal_elems: expression { $$ = vec<ast::ArrayLiteral>(ptr($1)); }
    | array_literal_elems ',' expression { $1.push_back(ptr($3)); $$ = std::move($1); }
    ;

%%

template <Operator::Unary Op, typename Operand>
ast::UnaryExpr make_unary(Operand&& operand) {
    return ast::UnaryExpr{Op, ptr(operand)};
}
template <Operator::Binary Op, typename Lhs, typename Rhs>
ast::BinaryExpr make_binary(Lhs&& lhs, Rhs&& rhs) {
    return ast::BinaryExpr{Op, ptr(lhs), ptr(rhs)};
}

std::optional<Type::ID> LexerContext::getType(llvm::StringRef name) const {
    if (const auto type = Type::from_name(name)) {
        return *type;
    }
    if (const auto record = this->instance.get().getTypeTable().get(name); 
        record.has_value()) {
        return record->get().typeId;
    }
    return std::nullopt;
}

bool field_already_exists(
    const ast::StructDef::Fields& fields, 
    const ast::StructDef::Field& theField) {
    return std::find_if(fields.begin(), fields.end(), 
        [&](const ast::StructDef::Field& field) {
            return field.name == theField.name;
        }) != fields.end();
}

template <typename T>
std::vector<std::remove_reference_t<T>> vec(T&& t) {
    std::vector<std::remove_reference_t<T>> v;
    v.push_back(std::move(t));
    return v;
}

template <typename W, typename T>
ast::VectorWrapper<std::remove_reference_t<T>, W> vec(T&& t) {
    ast::VectorWrapper<std::remove_reference_t<T>, W> v;
    v.push_back(std::move(t));
    return v;
}
