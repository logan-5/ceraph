#ifndef CERAPH_AST_HPP
#define CERAPH_AST_HPP

#include "operator.hpp"
#include "type.hpp"

#include <ostream>
#include <string>
#include <variant>

namespace ast {

struct Node;

using NodePtr = std::shared_ptr<Node>;
template <typename NodeType = Node, typename... Args>
NodePtr make_nodeptr(Args&&... args) {
    return std::make_shared<NodeType>(std::forward<Args>(args)...);
}

template <typename Rep, Type::ID Ty>
struct Literal {
    constexpr Literal() noexcept = default;
    template <typename T>
    Literal(T&& t) : rep{std::forward<T>(t)} {}

    static constexpr Type::ID getType() noexcept { return Ty; }
    Rep rep;
};
using StringLiteral = Literal<std::string, Type::ID::StringLiteral>;
using FloatLiteral = Literal<float, Type::ID::Float>;
using DoubleLiteral = Literal<double, Type::ID::Double>;

template <typename Rep, Type::ID Ty>
struct IntegerLiteral : Literal<Rep, Ty> {
    static_assert(Type::is_integer_v<Ty>);
    using Literal<Rep, Ty>::Literal;
    static constexpr unsigned getNumBits() noexcept {
        return Type::num_bits<Ty>::value;
    }
    static constexpr bool isSigned() noexcept {
        return Type::is_signed<Ty>::value;
    }
};

using IntLiteral = IntegerLiteral<int, Type::ID::Int>;

struct UnaryExpr {
    UnaryExpr(Operator::Unary in_op, NodePtr in_operand) noexcept
        : op{in_op}, operand{std::move(in_operand)} {}
    Operator::Unary op;
    NodePtr operand;
};

struct BinaryExpr {
    BinaryExpr(Operator::Binary in_op, NodePtr in_lhs, NodePtr in_rhs) noexcept
        : op{in_op}, lhs{std::move(in_lhs)}, rhs{std::move(in_rhs)} {}
    Operator::Binary op;
    NodePtr lhs;
    NodePtr rhs;
};

struct Node
    : std::variant<IntLiteral,
                   FloatLiteral,
                   DoubleLiteral,
                   StringLiteral,
                   UnaryExpr,
                   BinaryExpr> {
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

std::ostream& operator<<(std::ostream&, const ast::Node&);
}  // namespace ast
namespace llvm {
class raw_ostream;
}
namespace ast {
llvm::raw_ostream& operator<<(llvm::raw_ostream&, const ast::Node&);

}  // namespace ast

#endif
