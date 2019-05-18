#include "codegen.hpp"

#include "llvm/IR/Constants.h"
#include "llvm/IR/IRBuilder.h"
#include "llvm/IR/LLVMContext.h"
#include "llvm/IR/Module.h"
#include "llvm/IR/Verifier.h"

using llvm::IRBuilder;
using llvm::LLVMContext;
using llvm::Module;
using llvm::Value;

namespace codegen {

struct CodeGenInstance::Impl {
    LLVMContext context;
    IRBuilder<> builder{context};
    std::unique_ptr<Module> module{std::make_unique<Module>("module", context)};
    std::map<std::string, Value*> namedValues;
};

CodeGenInstance::CodeGenInstance() : impl{std::make_shared<Impl>()} {}

bool CodeGenInstance::verify(llvm::raw_ostream& ostr) const {
    return llvm::verifyModule(*impl->module, &ostr);
}

Value* Visitor::operator()(const ast::StringLiteral& str) const {
    return nullptr;
}

namespace {
Value* negate(CodeGenInstance::Impl& instance, Value* operand) {
    if (operand->getType()->isIntegerTy()) {
        return instance.builder.CreateNSWNeg(operand, "negatei");
    } else if (operand->getType()->isFloatingPointTy()) {
        return instance.builder.CreateFNeg(operand, "negatefp");
    }
    return nullptr;
}
}  // namespace

Value* Visitor::operator()(const ast::UnaryExpr& unary) const {
    Value* const operand = unary.operand->visit(*this);
    using namespace Operator;
    switch (unary.op) {
        case Unary::Minus:
            return negate(*instance.impl, operand);
    }
    return nullptr;
}

Value* Visitor::operator()(const ast::BinaryExpr& binary) const {
    Value* const lhs = binary.lhs->visit(*this);
    Value* const rhs = binary.rhs->visit(*this);
    using namespace Operator;
    switch (binary.op) {
        case Binary::Plus:
            return instance.impl->builder.CreateFAdd(lhs, rhs, "addfp");
        default:
            break;
    }
    return nullptr;
}

llvm::Value* Visitor::operator()(const ast::FunctionProto& proto) const {
    return nullptr;
}

Value* Visitor::make_floating_constant(Type::ID type,
                                       const llvm::APFloat& value) const {
    auto* const t = Type::get_type(type, instance.impl->context);
    assert(t);
    assert(t->isFloatingPointTy());
    return llvm::ConstantFP::get(t, value);
}
Value* Visitor::make_integer_constant(Type::ID type,
                                      const llvm::APInt& value) const {
    auto* const t = Type::get_type(type, instance.impl->context);
    assert(t);
    assert(t->isIntegerTy());
    return llvm::ConstantInt::get(t, value);
}

}  // namespace codegen