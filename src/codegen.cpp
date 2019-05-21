#include "codegen.hpp"

#include "scope.hpp"
#include "util.hpp"

#include "llvm/ADT/SmallVector.h"
#include "llvm/IR/Constants.h"
#include "llvm/IR/Function.h"
#include "llvm/IR/IRBuilder.h"
#include "llvm/IR/LLVMContext.h"
#include "llvm/IR/Module.h"
#include "llvm/IR/Verifier.h"

using llvm::Expected;
using llvm::IRBuilder;
using llvm::LLVMContext;
using llvm::Module;
using llvm::Value;

namespace {
bool isVoid(llvm::Value* v) {
    return !v || v->getType()->isVoidTy();
}
}  // namespace

#define DECLARE_OR_RETURN(NAME, INIT)       \
    auto NAME##OrErr = INIT;                \
    if (auto err = NAME##OrErr.takeError()) \
        return std::move(err);              \
    const auto& NAME = *NAME##OrErr

#define DECLARE_OR_RETURN_NOVOID(NAME, INIT)                    \
    DECLARE_OR_RETURN(NAME, INIT);                              \
    if (isVoid(NAME)) {                                         \
        return err("'void'-typed expression not allowed here"); \
    }

namespace codegen {

char CodeGenError::ID;
auto err(const llvm::Twine& desc) {
    return llvm::make_error<CodeGenError>(desc);
}

using ReturnType = Visitor::ReturnType;

struct CodeGenInstance::Impl {
    LLVMContext context;
    IRBuilder<> builder{context};
    std::unique_ptr<Module> module{std::make_unique<Module>("module", context)};
    scope::SymbolTable symtable;
};

CodeGenInstance::CodeGenInstance() : impl{std::make_shared<Impl>()} {}

bool CodeGenInstance::verify(llvm::raw_ostream& ostr) const {
    return llvm::verifyModule(*impl->module, &ostr);
}

ReturnType Visitor::operator()(const ast::StringLiteral& str) const {
    return nullptr;
}

namespace {
ReturnType negate(CodeGenInstance::Impl& instance, Value* operand) {
    if (operand->getType()->isIntegerTy()) {
        return instance.builder.CreateNSWNeg(operand, "negatei");
    } else if (operand->getType()->isFloatingPointTy()) {
        return instance.builder.CreateFNeg(operand, "negatef");
    }
    return nullptr;
}
}  // namespace

ReturnType Visitor::operator()(const ast::UnaryExpr& unary) const {
    DECLARE_OR_RETURN_NOVOID(operand, unary.operand->visit(*this));
    using namespace Operator;
    switch (unary.op) {
        case Unary::Minus:
            return negate(*instance.impl, operand);
    }
    return nullptr;
}

ReturnType Visitor::operator()(const ast::BinaryExpr& binary) const {
    DECLARE_OR_RETURN_NOVOID(lhs, binary.lhs->visit(*this));
    DECLARE_OR_RETURN_NOVOID(rhs, binary.rhs->visit(*this));
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
                return builder.CreateSDiv(lhs, rhs, "sdivi");  // TODO unsigned
            } else {
                return nullptr;
            }
        case Binary::Modulo:
            if (type->isFloatingPointTy()) {
                return builder.CreateFRem(lhs, rhs, "mulf");
            } else if (type->isIntegerTy()) {
                return builder.CreateSRem(lhs, rhs, "sremi");  // TODO unsigned
            } else {
                return nullptr;
            }

        case Binary::Less:
            if (type->isFloatingPointTy()) {
                return builder.CreateFCmpOLT(lhs, rhs, "cmp_oltf");
            } else if (type->isIntegerTy()) {
                return builder.CreateICmpSLT(lhs, rhs,
                                             "cmp_slti");  // TODO unsigned
            } else {
                return nullptr;
            }
        case Binary::Equality:
            if (type->isFloatingPointTy()) {
                return builder.CreateFCmpOEQ(lhs, rhs, "cmp_oeqf");
            } else if (type->isIntegerTy()) {
                return builder.CreateICmpEQ(lhs, rhs, "cmp_eqi");
            } else {
                return nullptr;
            }
    }
    return err("not yet implemented");
}

ReturnType Visitor::operator()(const ast::Identifier& ident) const {
    if (auto* const value = instance.impl->symtable.get(ident.name)) {
        auto* const load = instance.impl->builder.CreateLoad(value, ident.name);
        return load;
    }
    return err("identifier '" + ident.name + "' not found");
}

ReturnType Visitor::operator()(const ast::FunctionProto& proto) const {
    auto* const t = Type::get_type(proto, instance.impl->context);

    if (auto* const f = instance.impl->module->getFunction(proto.name);
        f && (f->getFunctionType() != t)) {
        return err('\'' + proto.name +
                   "' prototype redeclared with a different signature");
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

ReturnType Visitor::operator()(const ast::FunctionDef& func) const {
    auto* f = instance.impl->module->getFunction(func.proto.name);
    if (!f) {
        auto fOrError = this->operator()(func.proto);
        if (auto err = fOrError.takeError())
            return std::move(err);
        f = llvm::cast<llvm::Function>(*fOrError);
    }
    assert(f);
    if (!f->empty()) {
        return err("function '" + func.proto.name + "' redefined");
    }
    if (f->getFunctionType() !=
        Type::get_type(func.proto, instance.impl->context)) {
        return err("function '" + func.proto.name +
                   "' defined with a different signature than it was first "
                   "declared with");
    }

    auto& builder = instance.impl->builder;

    auto* const block =
          llvm::BasicBlock::Create(instance.impl->context, "entry", f);
    builder.SetInsertPoint(block);

    instance.impl->symtable.pushScope();
    util::ScopeGuard pop{[&] { instance.impl->symtable.popScope(); }};
    for (auto& arg : f->args()) {
        auto* const alloca =
              createAllocaInEntryBlock(arg.getType(), arg.getName());
        instance.impl->symtable.insert(arg.getName(), alloca);
        builder.CreateStore(&arg, alloca);
    }

    DECLARE_OR_RETURN(body, func.body->visit(*this));

    auto* const endBlock = builder.GetInsertBlock();
    if (!endBlock->getTerminator()) {
        if (Type::is_void(func.proto.returnType)) {
            builder.CreateRetVoid();
        } else {
            if (!body || body->getType() != f->getReturnType()) {
                return err("return type mismatch");
            }
            builder.CreateRet(body);
        }
    }

    return f;
}

ReturnType Visitor::operator()(const ast::FunctionCall& call) const {
    llvm::Function* f = instance.impl->module->getFunction(call.name);
    if (!f) {
        return err("unknown function '" + call.name + "'");
    }
    if (f->arg_size() != call.args.size()) {
        return err("argument count mismatch, expected " +
                   llvm::Twine(f->arg_size()) + ", have " +
                   llvm::Twine(call.args.size()));
    }
    llvm::SmallVector<Value*, 8> args;
    for (std::size_t i = 0; i < call.args.size(); ++i) {
        auto& argExpr = call.args[i];
        DECLARE_OR_RETURN_NOVOID(arg, argExpr->visit(*this));
        llvm::Argument& destArg = *std::next(f->arg_begin(), i);
        if (destArg.getType() != arg->getType()) {
            return err("type mismatch for argument " + llvm::Twine(i + 1));
        }
        args.push_back(arg);
    }

    return instance.impl->builder.CreateCall(
          f->getFunctionType(), f, args,
          f->getReturnType()->isVoidTy() ? "" : "call" + call.name);
}

ReturnType Visitor::operator()(const ast::IfElse& ifElse) const {
    DECLARE_OR_RETURN_NOVOID(cond, ifElse.cond->visit(*this));
    if (cond->getType() !=
        Type::get_type(Type::ID::Bool, instance.impl->context)) {
        return err("'if' condition is not a boolean expression");
    }

    auto& builder = instance.impl->builder;

    auto* const func = builder.GetInsertBlock()->getParent();

    auto* const thenBlock =
          llvm::BasicBlock::Create(instance.impl->context, "then", func);
    auto* const elseBlock =
          llvm::BasicBlock::Create(instance.impl->context, "else");
    auto* const mergeBlock =
          llvm::BasicBlock::Create(instance.impl->context, "merge");

    auto* const branchInst = builder.CreateCondBr(cond, thenBlock, elseBlock);
    builder.SetInsertPoint(thenBlock);
    DECLARE_OR_RETURN(thenBranch, ifElse.thenBranch->visit(*this));
    builder.CreateBr(mergeBlock);
    auto* const thenBlockEnd = builder.GetInsertBlock();

    func->getBasicBlockList().push_back(elseBlock);
    builder.SetInsertPoint(elseBlock);
    DECLARE_OR_RETURN(elseBranch, ifElse.elseBranch->visit(*this));
    const bool bothVoid = isVoid(thenBranch) && isVoid(elseBranch);
    if (!bothVoid && ((isVoid(thenBranch) != isVoid(elseBranch)) ||
                      (thenBranch->getType() != elseBranch->getType()))) {
        return err("incompatible types in 'if' and 'else' branches");
    }
    builder.CreateBr(mergeBlock);
    auto* const elseBlockEnd = builder.GetInsertBlock();

    func->getBasicBlockList().push_back(mergeBlock);
    builder.SetInsertPoint(mergeBlock);

    if (!bothVoid) {
        auto* const phi = builder.CreatePHI(thenBranch->getType(), 2, "phi");
        phi->addIncoming(thenBranch, thenBlockEnd);
        phi->addIncoming(elseBranch, elseBlockEnd);
        return phi;
    }
    return nullptr;
}

ReturnType Visitor::operator()(const ast::CrappyForLoop& loop) const {
    return err("todo");
    // auto& builder = instance.impl->builder;
    // auto& namedValues = instance.impl->namedValues;

    // DECLARE_OR_RETURN(init, loop.init->visit(*this));

    // auto* const func = builder.GetInsertBlock()->getParent();
    // auto* const preLoopBlock = builder.GetInsertBlock();
    // auto* const loopBlock =
    //       llvm::BasicBlock::Create(instance.impl->context, "loop", func);

    // builder.CreateBr(loopBlock);

    // builder.SetInsertPoint(loopBlock);
    // auto* const loopVar = builder.CreatePHI(init->getType(), 2, loop.induct);
    // loopVar->addIncoming(init, preLoopBlock);

    // auto* const oldVal = std::exchange(namedValues[loop.induct], loopVar);
    // util::ScopeGuard restore{[&] { namedValues[loop.induct] = oldVal; }};

    // DECLARE_OR_RETURN(cond, loop.cond->visit(*this));
    // if (cond->getType() !=
    //     Type::get_type(Type::ID::Bool, instance.impl->context)) {
    //     return err("loop condition is not a boolean expression");
    // }

    // auto* const bodyBlock =
    //       llvm::BasicBlock::Create(instance.impl->context, "body", func);
    // auto* const endBlock =
    //       llvm::BasicBlock::Create(instance.impl->context, "loopend", func);
    // builder.CreateCondBr(cond, bodyBlock, endBlock);

    // builder.SetInsertPoint(bodyBlock);
    // DECLARE_OR_RETURN(body, loop.body->visit(*this));

    // DECLARE_OR_RETURN(incr, loop.incr->visit(*this));
    // auto* const endBodyBlock = builder.GetInsertBlock();
    // loopVar->addIncoming(incr, endBodyBlock);
    // builder.CreateBr(loopBlock);

    // builder.SetInsertPoint(endBlock);
    // auto* const end = llvm::cantFail(this->operator()(ast::IntLiteral{0}));

    // return end;
}

ReturnType Visitor::operator()(const ast::NullStmt) const {
    return nullptr;
}

ReturnType Visitor::operator()(const ast::Block& block) const {
    if (block.stmts.empty())
        return nullptr;

    const auto lastStatementIt = std::prev(block.stmts.end());
    for (auto it = block.stmts.begin(); it != lastStatementIt; ++it) {
        DECLARE_OR_RETURN(stmt, (*it)->visit(*this));
        (void)stmt;
    }

    DECLARE_OR_RETURN(stmt, (*lastStatementIt)->visit(*this));
    return stmt;
}

ReturnType Visitor::operator()(const ast::Declaration& decl) const {
    DECLARE_OR_RETURN(init, decl.init->visit(*this));
    if (!init || init->getType()->isVoidTy()) {
        return err("variable initializer cannot have void type");
    }
    if (decl.type.has_value() &&
        Type::get_type(*decl.type, instance.impl->context) != init->getType()) {
        return err(
              "type of initializer does not match variable's specified type");
    }
    auto* const t = init->getType();

    auto& builder = instance.impl->builder;
    auto* const alloca = createAllocaInEntryBlock(t, decl.name);
    instance.impl->symtable.insert(decl.name, alloca);

    auto* store = builder.CreateStore(init, alloca);

    return store;
}

ReturnType Visitor::operator()(const ast::Assignment& assign) const {
    DECLARE_OR_RETURN_NOVOID(rhs, assign.rhs->visit(*this));
    auto& builder = instance.impl->builder;
    auto& symtable = instance.impl->symtable;

    auto* const alloca = symtable.get(assign.dest);
    if (!alloca) {
        return err(llvm::Twine("assignment to unknown identifier '") +
                   assign.dest + "'");
    }
    if (alloca->getAllocatedType() != rhs->getType()) {
        return err("cannot assign new value, type mismatch");
    }
    builder.CreateStore(rhs, alloca);
    return rhs;
}

/////

llvm::AllocaInst* Visitor::createAllocaInEntryBlock(
      llvm::Type* type,
      const llvm::Twine& name) const {
    auto* const func = instance.impl->builder.GetInsertBlock()->getParent();
    IRBuilder entryBuilder{&func->getEntryBlock(),
                           func->getEntryBlock().begin()};
    return entryBuilder.CreateAlloca(type, 0, name);
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