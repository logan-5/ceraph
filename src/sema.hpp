#ifndef SEMA_HPP
#define SEMA_HPP

#include "ast.hpp"
#include "scope.hpp"
#include "type.hpp"

#include "llvm/Support/Error.h"

#include <optional>

namespace sema {

namespace detail {
struct None {
    None() = default;
    template <typename T>
    operator std::optional<T>() const {
        return std::nullopt;
    }
};
}  // namespace detail

struct TypeCheckError : public llvm::ErrorInfo<TypeCheckError> {
    static char ID;
    std::string description;

    TypeCheckError(const llvm::Twine& desc) : description{desc.str()} {}
    void log(llvm::raw_ostream& os) const override { os << description; }
    std::error_code convertToErrorCode() const override { return {}; }
};

struct GetType {
    std::reference_wrapper<const Type::UserDefinedTypeTable> typeTable;
    std::unique_ptr<
          scope::SymbolTable<std::optional<Type::CompoundType>, detail::None>>
          symbols;

    explicit GetType(const Type::UserDefinedTypeTable& in_typeTable)
        : typeTable{in_typeTable}
        , symbols{std::make_unique<
                scope::SymbolTable<std::optional<Type::CompoundType>,
                                   detail::None>>()} {}

    using ReturnType = llvm::Expected<Type::CompoundType>;

    template <typename Rep, Type::ID Ty>
    ReturnType operator()(const ast::Literal<Rep, Ty>&) const {
        return Ty;
    }

    ReturnType operator()(const ast::Identifier& ident) const;

    ReturnType operator()(const ast::UnaryExpr& unary) const;
    ReturnType operator()(const ast::BinaryExpr& binary) const;
    ReturnType operator()(const ast::FunctionProto& proto) const;
    ReturnType operator()(const ast::FunctionDef& func) const;
    ReturnType operator()(const ast::FunctionCall& call) const;

    ReturnType operator()(const ast::IfElse& ifElse) const;
    ReturnType operator()(const ast::While& while_) const;

    ReturnType operator()(const ast::LogicalAnd& a) const;
    ReturnType operator()(const ast::LogicalOr& o) const;

    ReturnType operator()(const ast::NullStmt nullStmt) const;
    ReturnType operator()(const ast::Block& block) const;

    ReturnType operator()(const ast::Declaration& decl) const;
    ReturnType operator()(const ast::Assignment& assign) const;

    ReturnType operator()(const ast::Return& ret) const;

    ReturnType operator()(const ast::StructValue& v) const;
    ReturnType operator()(const ast::StructMemberAccess& m) const;

    ReturnType operator()(const ast::ExplicitCast& cast) const;

    ReturnType operator()(const ast::AddressOf& addr) const;
    ReturnType operator()(const ast::Dereference& deref) const;

    ReturnType operator()(const ast::Subscript& ss) const;

   private:
    ReturnType ret(const std::optional<Type::CompoundType> ty,
                   const llvm::Twine& errorMsg) const;
};

enum class ValueCategory {
    None,
    LValue,
    RValue,
};

struct GetValueCategory {
    template <typename Rep, Type::ID Ty>
    ValueCategory operator()(const ast::Literal<Rep, Ty>&) const {
        return ValueCategory::RValue;
    }

    ValueCategory operator()(const ast::Identifier& ident) const;

    ValueCategory operator()(const ast::UnaryExpr& unary) const;
    ValueCategory operator()(const ast::BinaryExpr& binary) const;
    ValueCategory operator()(const ast::FunctionProto& proto) const;
    ValueCategory operator()(const ast::FunctionDef& func) const;
    ValueCategory operator()(const ast::FunctionCall& call) const;

    ValueCategory operator()(const ast::IfElse& ifElse) const;
    ValueCategory operator()(const ast::While& while_) const;

    ValueCategory operator()(const ast::LogicalAnd& a) const;
    ValueCategory operator()(const ast::LogicalOr& o) const;

    ValueCategory operator()(const ast::NullStmt nullStmt) const;
    ValueCategory operator()(const ast::Block& block) const;

    ValueCategory operator()(const ast::Declaration& decl) const;
    ValueCategory operator()(const ast::Assignment& assign) const;

    ValueCategory operator()(const ast::Return& ret) const;

    ValueCategory operator()(const ast::StructValue& v) const;
    ValueCategory operator()(const ast::StructMemberAccess& m) const;

    ValueCategory operator()(const ast::ExplicitCast& c) const;

    ValueCategory operator()(const ast::AddressOf& addr) const;
    ValueCategory operator()(const ast::Dereference& deref) const;

    ValueCategory operator()(const ast::Subscript& deref) const;
};

}  // namespace sema

#endif
