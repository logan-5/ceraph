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

    void operator()(const NullLiteral n) const {
        indent();
        ostr << "null";
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

    void operator()(const While& while_) const {
        indent();
        ostr << "[while]";
        Dump next{indentLevel + 1, ostr};
        if (while_.init) {
            ostr << "init:\n";
            std::visit(next, *while_.init);
            ostr << '\n';
            indent();
        }
        ostr << "condition:\n";
        std::visit(next, *while_.cond);
        ostr << '\n';
        indent();
        ostr << "body:\n";
        std::visit(next, *while_.body);
    }

    void operator()(NullStmt) const {
        indent();
        ostr << "[null statement]";
    }

    void operator()(const Block& block) const {
        indent();
        ostr << "[block]:\n";
        Dump next{indentLevel + 1, ostr};
        for (auto& stmt : block.stmts) {
            std::visit(next, *stmt);
            ostr << '\n';
        }
    }

    void operator()(const Declaration& decl) const {
        indent();
        ostr << "[declaration]: var '" << decl.name << "', init:\n";
        Dump next{indentLevel + 1, ostr};
        std::visit(next, *decl.init);
    }

    void operator()(const Assignment& assign) const {
        indent();
        ostr << "[assignment]: var '\n";
        Dump next{indentLevel + 1, ostr};
        std::visit(next, *assign.dest);
        ostr << "', new value:\n";
        std::visit(next, *assign.rhs);
    }

    void operator()(const Return& ret) const {
        indent();
        ostr << "[return]";
        if (ret.value) {
            ostr << ":\n";
            Dump next{indentLevel + 1, ostr};
            std::visit(next, *ret.value);
        }
    }

    void operator()(const LogicalAnd& a) const {
        indent();
        ostr << "[and]:\n";
        Dump next{indentLevel + 1, ostr};
        std::visit(next, *a.lhs);
        ostr << '\n';
        std::visit(next, *a.rhs);
    }
    void operator()(const LogicalOr& o) const {
        indent();
        ostr << "[or]:\n";
        Dump next{indentLevel + 1, ostr};
        std::visit(next, *o.lhs);
        ostr << '\n';
        std::visit(next, *o.rhs);
    }

    void operator()(const StructDef& d) const {
        indent();
        ostr << "struct def: " << d.name << '\n';
        for (auto& field : d.fields) {
            indent();
            ostr << "\t[field]: " << field.name << ", type: " << field.type
                 << '\n';
        }
    }
    void operator()(const StructValue& v) const {
        indent();
        ostr << "struct literal: " << Type::to_string(v.type);
    }

    void operator()(const StructMemberAccess& a) const {
        indent();
        ostr << "member access:\n";
        Dump next{indentLevel + 1, ostr};
        std::visit(next, *a.lhs);
        ostr << '\n';
        indent();
        ostr << "member: " << a.rhs;
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

std::ostream& operator<<(std::ostream& ostr, const ast::StructDef& sd) {
    Dump{0, ostr}(sd);
    return ostr;
}

llvm::raw_ostream& operator<<(llvm::raw_ostream& ostr,
                              const ast::StructDef& sd) {
    Dump{0, ostr}(sd);
    return ostr;
}

}  // namespace ast
