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

    void operator()(const Identifier& ident) const {
        indent();
        ostr << "[ident] " << ident.name;
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
        ostr << '[' << proto.returnType << "] ";
        ostr << proto.name;
        ostr << '(';
        bool first = true;
        for (auto& arg : proto.args) {
            if (!std::exchange(first, false))
                ostr << ", ";
            ostr << '[' << arg.type << "] ";
            if (arg.name)
                ostr << *arg.name;
        }
        ostr << ')';
    }

    void operator()(const FunctionDef& func) const {
        indent();
        ostr << "[function]: " << func.proto;
        Dump next{indentLevel + 1, ostr};
        std::visit(next, *func.body);
    }

    void operator()(const FunctionCall& call) const {
        indent();
        ostr << "[function call]: " << call.name;
        if (!call.args.empty()) {
            Dump next{indentLevel + 1, ostr};
            ostr << ", with args:\n";
            for (auto& arg : call.args)
                std::visit(next, *arg);
        }
    }

    void operator()(const IfElse& ifElse) const {
        indent();
        ostr << '[' << (ifElse.elseBranch ? "if/else" : "if") << ']';
        Dump next{indentLevel + 1, ostr};
        ostr << " condition:\n";
        std::visit(next, *ifElse.cond);
        ostr << '\n';
        indent();
        ostr << "then:\n";
        std::visit(next, *ifElse.thenBranch);
        if (ifElse.elseBranch) {
            ostr << '\n';
            indent();
            ostr << "else:\n";
            std::visit(next, *ifElse.elseBranch);
        }
    }

    void operator()(const CrappyForLoop&) const { ostr << "crappy 'for' loop"; }
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
