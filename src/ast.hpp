#ifndef CERAPH_AST_HPP
#define CERAPH_AST_HPP

#include "cmp_ops.hpp"
#include "operator.hpp"
#include "type.hpp"
#include "util.hpp"

#include <ostream>
#include <string>
#include <variant>

namespace ast {

struct Node;

template <typename T, typename Tag>
struct VectorWrapper : public std::vector<T> {
    using std::vector<T>::vector;
};

using NodePtr = std::shared_ptr<Node>;
template <typename... Args>
NodePtr make_nodeptr(Args&&... args) {
    return std::make_shared<Node>(std::forward<Args>(args)...);
}

template <typename Rep_, Type::ID Ty>
struct Literal {
    using Rep = Rep_;

    template <typename T>
    explicit Literal(T&& t) : rep{std::forward<T>(t)} {}

    static constexpr Type::ID getType() noexcept { return Ty; }
    Rep rep;

    bool operator<(const Literal& rhs) const { return rep < rhs.rep; }
    bool operator==(const Literal& rhs) const { return rep == rhs.rep; }

   protected:
    explicit constexpr Literal() noexcept = default;
};

using StringLiteral = Literal<std::string, Type::ID::StringLiteral>;
using FloatLiteral = Literal<float, Type::ID::Float>;
using DoubleLiteral = Literal<double, Type::ID::Double>;

// TODO some kinda EBO wrapper for Literal's so empty Rep's don't take up space
struct NullLiteral : Literal<std::monostate, Type::ID::Null> {
    explicit NullLiteral() : Literal{Literal::Rep{}} {}

    using Literal::getType;
};

template <typename Rep, Type::ID Ty>
struct IntegerLiteral : Literal<Rep, Ty> {
   private:
    using base = Literal<Rep, Ty>;

   public:
    static_assert(Type::is_integer(Ty));
    using base::Literal;

    static constexpr unsigned getNumBits() noexcept {
        return Type::num_bits<Ty>::value;
    }
    static constexpr bool isSigned() noexcept {
        return Type::is_signed<Ty>::value;
    }
};

using BoolLiteral = IntegerLiteral<bool, Type::ID::Bool>;
using IntLiteral = IntegerLiteral<int, Type::ID::Int>;

struct ArrayLiteral {
    using Elems = VectorWrapper<NodePtr, ArrayLiteral>;
    Elems elems;
};
CMP_OPS(ArrayLiteral, elems);

struct Identifier {
    std::string name;
};
CMP_OPS(Identifier, name);

struct UnaryExpr {
    UnaryExpr(Operator::Unary in_op, NodePtr in_operand) noexcept
        : op{in_op}, operand{std::move(in_operand)} {}
    Operator::Unary op;
    NodePtr operand;
};
CMP_OPS(UnaryExpr, op, operand);

struct BinaryExpr {
    BinaryExpr(Operator::Binary in_op, NodePtr in_lhs, NodePtr in_rhs) noexcept
        : op{in_op}, lhs{std::move(in_lhs)}, rhs{std::move(in_rhs)} {}
    Operator::Binary op;
    NodePtr lhs;
    NodePtr rhs;
};
CMP_OPS(BinaryExpr, op, lhs, rhs);

struct FunctionProto {
    Type::CompoundType returnType;
    std::string name;

    struct Arg {
        Type::CompoundType type;
        std::optional<std::string> name;
    };
    using ArgList = std::vector<Arg>;
    ArgList args;
};
CMP_OPS(FunctionProto::Arg, type, name);
CMP_OPS(FunctionProto, returnType, name, args);

struct FunctionDef {
    FunctionProto proto;
    NodePtr body;
};
CMP_OPS(FunctionDef, proto, body);

struct FunctionCall {
    std::string name;
    using Arg = NodePtr;
    using ArgList = std::vector<Arg>;
    ArgList args;
};
CMP_OPS(FunctionCall, name, args);

struct IfElse {
    IfElse(NodePtr in_cond, NodePtr in_then)
        : cond{std::move(in_cond)}, thenBranch{std::move(in_then)} {}
    IfElse(NodePtr in_cond, NodePtr in_then, NodePtr in_else)
        : cond{std::move(in_cond)}
        , thenBranch{std::move(in_then)}
        , elseBranch{std::move(in_else)} {}
    NodePtr cond;
    NodePtr thenBranch;
    NodePtr elseBranch;
};
CMP_OPS(IfElse, cond, thenBranch, elseBranch);

struct While {
    NodePtr init;
    NodePtr cond;
    NodePtr body;
};
CMP_OPS(While, init, cond, body);

using NullStmt = std::monostate;

struct Block {
    using Stmts = VectorWrapper<NodePtr, Block>;
    Stmts stmts;
};
CMP_OPS(Block, stmts);

struct Declaration {
    std::optional<Type::CompoundType> type;
    std::string name;
    NodePtr init;
};
CMP_OPS(Declaration, type, name, init);

struct Assignment {
    NodePtr dest;
    NodePtr rhs;
};
CMP_OPS(Assignment, dest, rhs);

struct Return {
    NodePtr value;
};
CMP_OPS(Return, value);

struct LogicalAnd {
    NodePtr lhs, rhs;
};
CMP_OPS(LogicalAnd, lhs, rhs);

struct LogicalOr {
    NodePtr lhs, rhs;
};
CMP_OPS(LogicalOr, lhs, rhs);

struct StructValue {
    Type::ID type;
};
CMP_OPS(StructValue, type);

struct StructMemberAccess {
    NodePtr lhs;
    std::string rhs;
    bool dereference;
};
CMP_OPS(StructMemberAccess, lhs, rhs, dereference);

struct ExplicitCast {
    NodePtr operand;
    Type::CompoundType toType;
};
CMP_OPS(ExplicitCast, operand, toType);

struct AddressOf {
    NodePtr operand;
};
CMP_OPS(AddressOf, operand);

struct Dereference {
    NodePtr operand;
};
CMP_OPS(Dereference, operand);

struct Subscript {
    NodePtr lhs;
    NodePtr rhs;
};
CMP_OPS(Subscript, lhs, rhs);

struct Node
    : std::variant<NullStmt,
                   BoolLiteral,
                   IntLiteral,
                   FloatLiteral,
                   DoubleLiteral,
                   StringLiteral,
                   NullLiteral,
                   ArrayLiteral,
                   Identifier,
                   UnaryExpr,
                   BinaryExpr,
                   FunctionProto,
                   FunctionDef,
                   FunctionCall,
                   IfElse,
                   While,
                   LogicalAnd,
                   LogicalOr,
                   Block,
                   Declaration,
                   Assignment,
                   Return,
                   StructValue,
                   StructMemberAccess,
                   ExplicitCast,
                   AddressOf,
                   Dereference,
                   Subscript> {
    using variant::variant;

    template <typename Visitor>
    decltype(auto) visit(Visitor&& v) const {
        return std::visit(std::forward<Visitor>(v), *this);
    }
    template <typename Visitor>
    decltype(auto) visit(Visitor&& v) {
        return std::visit(std::forward<Visitor>(v), *this);
    }
};

struct StructDef {
    std::string name;

    struct Field {
        std::string name;
        Type::CompoundType type;
    };
    using Fields = std::vector<Field>;
    Fields fields;
};

std::ostream& operator<<(std::ostream&, const ast::Node&);
std::ostream& operator<<(std::ostream&, const ast::StructDef&);
}  // namespace ast
namespace llvm {
class raw_ostream;
}
namespace ast {
llvm::raw_ostream& operator<<(llvm::raw_ostream&, const ast::Node&);
llvm::raw_ostream& operator<<(llvm::raw_ostream&, const ast::StructDef&);

}  // namespace ast

#endif
