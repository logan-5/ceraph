#include "ast.hpp"

namespace ast {

template <typename OStream>
struct Dump {
    unsigned indentLevel;
    OStream& ostr;

    void indent() const {
        for (unsigned i = 0; i < indentLevel; ++i) {
            ostr << '\t';
        }
    }

    template <typename Rep, Type::ID Ty>
    void operator()(const Literal<Rep, Ty>& lit) const {
        indent();
        ostr << lit.getType() << ": " << lit.rep;
    }

    void operator()(const UnaryExpr& expr) const {
        indent();
        ostr << expr.op;
        Dump next{indentLevel + 1, ostr};
        ostr << '\n';
        std::visit(next, *expr.operand);
    }
    void operator()(const BinaryExpr& expr) const {
        indent();
        ostr << expr.op;
        Dump next{indentLevel + 1, ostr};
        ostr << '\n';
        std::visit(next, *expr.lhs);
        ostr << '\n';
        std::visit(next, *expr.rhs);
    }
};
template <typename OStream>
Dump(unsigned, OStream&)->Dump<OStream>;

std::ostream& operator<<(std::ostream& ostr, const ast::Node& node) {
    Dump dump{0, ostr};
    std::visit(dump, node);
    return ostr;
}

}  // namespace ast
