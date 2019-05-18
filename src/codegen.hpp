#ifndef CERAPH_CODEGEN_HPP
#define CERAPH_CODEGEN_HPP

#include "ast.hpp"
#include "type.hpp"

#include "llvm/ADT/APFloat.h"
#include "llvm/IR/Value.h"

namespace codegen {

struct CodeGenInstance {
    CodeGenInstance();

    bool verify(llvm::raw_ostream&) const;

    struct Impl;
    std::shared_ptr<Impl> impl;
};

struct Visitor {
    CodeGenInstance& instance;

    template <typename Rep, Type::ID Ty>
    std::enable_if_t<Type::is_floating_v<Ty>, llvm::Value*> operator()(
          const ast::Literal<Rep, Ty>& literal) const {
        return make_floating_constant(Ty, llvm::APFloat{literal.rep});
    }
    template <typename Rep, Type::ID Ty>
    llvm::Value* operator()(const ast::IntegerLiteral<Rep, Ty>& literal) const {
        return make_integer_constant(
              Ty, llvm::APInt{literal.getNumBits(),
                              /* TODO I don't get this yet */
                              static_cast<std::uint64_t>(literal.rep),
                              literal.isSigned()});
    }

    llvm::Value* operator()(const ast::StringLiteral& str) const;

    llvm::Value* operator()(const ast::Identifier& ident) const;

    llvm::Value* operator()(const ast::UnaryExpr& unary) const;
    llvm::Value* operator()(const ast::BinaryExpr& binary) const;
    llvm::Value* operator()(const ast::FunctionProto& proto) const;
    llvm::Value* operator()(const ast::FunctionDef& func) const;

   private:
    llvm::Value* make_floating_constant(Type::ID type,
                                        const llvm::APFloat& value) const;
    llvm::Value* make_integer_constant(Type::ID type,
                                       const llvm::APInt& value) const;
};

}  // namespace codegen

#endif
