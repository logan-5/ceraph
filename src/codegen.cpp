#include "codegen.hpp"

#include "llvm/ADT/SmallVector.h"
#include "llvm/IR/Constants.h"
#include "llvm/IR/Function.h"
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
    llvm::StringMap<Value*> namedValues;
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
        return instance.builder.CreateFNeg(operand, "negatef");
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
    assert(lhs);
    assert(rhs);
    assert(lhs->getType() ==
           rhs->getType());  // TODO implicit conversions in the front(er)end?
    llvm::Type* const type = lhs->getType();
    auto& builder = instance.impl->builder;
    using namespace Operator;
    switch (binary.op) {
        case Binary::Plus:
            if (type->isFloatingPointTy()) {
                return builder.CreateFAdd(lhs, rhs, "addf");
            } else if (type->isIntegerTy()) {
                return builder.CreateAdd(lhs, rhs, "addi");
            } else {
                return nullptr;
            }
        case Binary::Minus:
            if (type->isFloatingPointTy()) {
                return builder.CreateFSub(lhs, rhs, "subf");
            } else if (type->isIntegerTy()) {
                return builder.CreateSub(lhs, rhs, "subi");
            } else {
                return nullptr;
            }
        case Binary::Multiply:
            if (type->isFloatingPointTy()) {
                return builder.CreateFMul(lhs, rhs, "mulf");
            } else if (type->isIntegerTy()) {
                return builder.CreateMul(lhs, rhs, "muli");
            } else {
                return nullptr;
            }
        case Binary::Divide:
            if (type->isFloatingPointTy()) {
                return builder.CreateFDiv(lhs, rhs, "divf");
            } else if (type->isIntegerTy()) {
                return builder.CreateSDiv(lhs, rhs, "divi");  // TODO unsigned
            } else {
                return nullptr;
            }
        case Binary::Modulo:
            if (type->isFloatingPointTy()) {
                return builder.CreateFRem(lhs, rhs, "mulf");
            } else if (type->isIntegerTy()) {
                return builder.CreateSRem(lhs, rhs, "remi");  // TODO unsigned
            } else {
                return nullptr;
            }
    }
    return nullptr;
}

llvm::Value* Visitor::operator()(const ast::Identifier& ident) const {
    // TODO error handling
    if (auto it = instance.impl->namedValues.find(ident.name);
        it != instance.impl->namedValues.end()) {
        return it->getValue();
    }
    assert(false && "nope");
    return nullptr;
}

llvm::Value* Visitor::operator()(const ast::FunctionProto& proto) const {
    auto* const t = Type::get_type(proto, instance.impl->context);

    if (auto* const f = instance.impl->module->getFunction(proto.name)) {
        assert(
              f->getFunctionType() == t &&
              "prototype redeclared with a different signature");  // TODO error
                                                                   // handling
        return f;
    }

    auto* const f =
          llvm::Function::Create(t, llvm::Function::ExternalLinkage, proto.name,
                                 instance.impl->module.get());

    assert(f->arg_size() == proto.args.size());
    std::size_t idx = 0;
    for (auto& arg : f->args()) {
        if (auto& name = proto.args[idx++].name)
            arg.setName(*name);
    }

    return f;
}

llvm::Value* Visitor::operator()(const ast::FunctionDef& func) const {
    auto* const f =
          instance.impl->module->getFunction(func.proto.name)
                ?: llvm::cast<llvm::Function>(this->operator()(func.proto));
    assert(f);
    assert(f->empty() && "function redefined");  // TODO error handling
    assert(f->getFunctionType() ==
                 Type::get_type(func.proto, instance.impl->context) &&
           "function redefined with a different signature");  // TODO error
                                                              // handling

    auto& builder = instance.impl->builder;

    auto* const block =
          llvm::BasicBlock::Create(instance.impl->context, "entry", f);
    builder.SetInsertPoint(block);

    instance.impl->namedValues.clear();
    for (auto& arg : f->args())
        instance.impl->namedValues[arg.getName()] = &arg;

    auto* const body = func.body->visit(*this);

    if (!block->getTerminator() && Type::is_void(func.proto.returnType)) {
        builder.CreateRetVoid();
    } else {
        builder.CreateRet(body);
        // TODO handle error -- missing return statement
    }

    return f;
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