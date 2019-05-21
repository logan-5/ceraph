#ifndef CERAPH_CODEGEN_HPP
#define CERAPH_CODEGEN_HPP

#include "ast.hpp"
#include "type.hpp"

#include "llvm/ADT/APFloat.h"
#include "llvm/IR/Value.h"
#include "llvm/Support/Error.h"

namespace codegen {

struct CodeGenInstance {
    CodeGenInstance();

    bool verify(llvm::raw_ostream&) const;

    struct Impl;
    std::shared_ptr<Impl> impl;
};

struct CodeGenError : public llvm::ErrorInfo<CodeGenError> {
    static char ID;
    std::string description;

    CodeGenError(const llvm::Twine& desc) : description{desc.str()} {}
    void log(llvm::raw_ostream& os) const override { os << description; }
    std::error_code convertToErrorCode() const override { return {}; }
};

struct Visitor {
    using ReturnType = llvm::Expected<
          llvm::Value*>;  // TODO remove awkward nullptr state:
                          // llvm::Expected<std::reference_wrapper<llvm::Value>>;

    CodeGenInstance& instance;

    template <typename Rep, Type::ID Ty>
    std::enable_if_t<Type::is_floating_v<Ty>, ReturnType> operator()(
          const ast::Literal<Rep, Ty>& literal) const {
        return make_floating_constant(Ty, llvm::APFloat{literal.rep});
    }
    template <typename Rep, Type::ID Ty>
    ReturnType operator()(const ast::IntegerLiteral<Rep, Ty>& literal) const {
        return make_integer_constant(
              Ty, llvm::APInt{literal.getNumBits(),
                              /* TODO I don't get this yet */
                              static_cast<std::uint64_t>(literal.rep),
                              literal.isSigned()});
    }

    ReturnType operator()(const ast::StringLiteral& str) const;

    ReturnType operator()(const ast::Identifier& ident) const;

    ReturnType operator()(const ast::UnaryExpr& unary) const;
    ReturnType operator()(const ast::BinaryExpr& binary) const;
    ReturnType operator()(const ast::FunctionProto& proto) const;
    ReturnType operator()(const ast::FunctionDef& func) const;
    ReturnType operator()(const ast::FunctionCall& call) const;

    ReturnType operator()(const ast::IfElse& ifElse) const;

    ReturnType operator()(const ast::CrappyForLoop& loop) const;

    ReturnType operator()(const ast::NullStmt nullStmt) const;
    ReturnType operator()(const ast::Block& block) const;

   private:
    llvm::Value* make_floating_constant(Type::ID type,
                                        const llvm::APFloat& value) const;
    llvm::Value* make_integer_constant(Type::ID type,
                                       const llvm::APInt& value) const;
};

}  // namespace codegen

#endif
