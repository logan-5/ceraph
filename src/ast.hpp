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
using IntLiteral = Literal<int, Type::ID::Int>;
using FloatLiteral = Literal<float, Type::ID::Float>;
using DoubleLiteral = Literal<double, Type::ID::Double>;

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
};

std::ostream& operator<<(std::ostream&, const ast::Node&);

}  // namespace ast

#endif
