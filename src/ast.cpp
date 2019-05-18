#include "ast.hpp"

#include "llvm/Support/raw_ostream.h"

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

    void operator()(const FunctionProto& proto) const {
        indent();
        ostr << proto.name;
        ostr << '(';
        for (auto& arg : proto.args) {
            ostr << arg << ", ";
        }
        ostr << ')';
    }
};
template <typename OStream>
Dump(unsigned, OStream&)->Dump<OStream>;

std::ostream& operator<<(std::ostream& ostr, const ast::Node& node) {
    Dump dump{0, ostr};
    node.visit(dump);
    return ostr;
}

llvm::raw_ostream& operator<<(llvm::raw_ostream& ostr, const ast::Node& node) {
    Dump dump{0, ostr};
    node.visit(dump);
    return ostr;
}

}  // namespace ast
