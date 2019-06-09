#include "codegen.hpp"

#include "scope.hpp"
#include "sema.hpp"
#include "type.hpp"
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

bool breaksOrReturns(const llvm::BasicBlock* const block) {
    return block->getTerminator();
}

std::string str(llvm::Type* type) {
    std::string s;
    llvm::raw_string_ostream ostr{s};
    type->print(ostr);
    return s;
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

char CodeGenError::ID = 0;
auto err(const llvm::Twine& desc) {
    return llvm::make_error<CodeGenError>(desc);
}

using ReturnType = Visitor::ReturnType;

struct CodeGenInstance::Impl {
    LLVMContext context;
    IRBuilder<> builder{context};
    std::unique_ptr<Module> module{std::make_unique<Module>("module", context)};
    scope::SymbolTable<llvm::AllocaInst*> symtable;
    Type::UserDefinedTypeTable typeTable{context};

    llvm::Function* getCurrentFunction() const {
        return builder.GetInsertBlock()->getParent();
    }
    llvm::Type* getCurrentReturnType() const {
        return getCurrentFunction()->getReturnType();
    }
};

CodeGenInstance::CodeGenInstance() : impl{std::make_unique<Impl>()} {}
CodeGenInstance::~CodeGenInstance() = default;

const Type::UserDefinedTypeTable& CodeGenInstance::getTypeTable() const {
    return impl->typeTable;
}

bool CodeGenInstance::verify(llvm::raw_ostream& ostr) const {
    return llvm::verifyModule(*impl->module, &ostr);
}
void CodeGenInstance::dump(llvm::raw_ostream& ostr) const {
    impl->module->print(ostr, nullptr);
}

ReturnType Visitor::operator()(const ast::StringLiteral& str) const {
    return nullptr;
}

ReturnType Visitor::operator()(const ast::NullLiteral) const {
    auto* const zero = cantFail(this->operator()(ast::IntLiteral{0}));
    auto* const nullType = Type::get_type(
          Type::ID::Null, instance.impl->context, instance.impl->typeTable);
    return instance.impl->builder.CreateIntToPtr(zero, nullType, "null");
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
    DECLARE_OR_RETURN_NOVOID(operand, load(unary.operand->visit(*this)));
    using namespace Operator;
    switch (unary.op) {
        case Unary::Minus:
            return negate(*instance.impl, operand);
    }
    return nullptr;
}

ReturnType Visitor::operator()(const ast::BinaryExpr& binary) const {
    DECLARE_OR_RETURN_NOVOID(lhs, load(binary.lhs->visit(*this)));
    DECLARE_OR_RETURN_NOVOID(rhs, load(binary.rhs->visit(*this)));
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
        return value;
    }
    return err("identifier '" + ident.name + "' not found");
}

ReturnType Visitor::operator()(const ast::FunctionProto& proto) const {
    auto* const t = Type::get_type(proto, instance.impl->context,
                                   instance.impl->typeTable);

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
    if (f->getFunctionType() != Type::get_type(func.proto,
                                               instance.impl->context,
                                               instance.impl->typeTable)) {
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

    DECLARE_OR_RETURN(body, load(func.body->visit(*this)));

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
        DECLARE_OR_RETURN_NOVOID(arg, load(argExpr->visit(*this)));
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

namespace {
ReturnType setUpMergeBlock(llvm::Value* const thenValue,
                           llvm::BasicBlock* const thenBlock,
                           llvm::Value* const elseValue,
                           llvm::BasicBlock* const elseBlock,
                           llvm::BasicBlock* const mergeBlock,
                           IRBuilder<>& builder) {
    const bool thenReturnsOrBreaks = breaksOrReturns(thenBlock);
    const bool hasElseValue = elseValue != nullptr;
    const bool elseReturnsOrBreaks = hasElseValue && breaksOrReturns(elseBlock);
    if (thenReturnsOrBreaks && (hasElseValue && !elseReturnsOrBreaks)) {
        builder.SetInsertPoint(elseBlock);
        builder.CreateBr(mergeBlock);
        builder.SetInsertPoint(mergeBlock);
        auto* const phi =
              builder.CreatePHI(elseValue->getType(), 1, "dummyphi");
        phi->addIncoming(elseValue, elseBlock);
        return phi;
    }
    if (!thenReturnsOrBreaks && elseReturnsOrBreaks) {
        builder.SetInsertPoint(thenBlock);
        builder.CreateBr(mergeBlock);
        if (!hasElseValue && !elseBlock->getTerminator()) {
            builder.SetInsertPoint(elseBlock);
            builder.CreateBr(mergeBlock);
        }
        builder.SetInsertPoint(mergeBlock);
        auto* const phi =
              builder.CreatePHI(thenValue->getType(), 1, "dummyphi");
        phi->addIncoming(thenValue, thenBlock);
        return phi;
    }

    const bool bothVoid = isVoid(thenValue) && isVoid(elseValue);
    if (!bothVoid && ((isVoid(thenValue) != isVoid(elseValue)) ||
                      (thenValue->getType() != elseValue->getType()))) {
        return err("incompatible types in 'if' and 'else' branches");
    }

    if (!bothVoid) {
        builder.SetInsertPoint(thenBlock);
        builder.CreateBr(mergeBlock);
        builder.SetInsertPoint(elseBlock);
        builder.CreateBr(mergeBlock);

        builder.SetInsertPoint(mergeBlock);
        auto* const phi = builder.CreatePHI(thenValue->getType(), 2, "phi");
        phi->addIncoming(thenValue, thenBlock);
        phi->addIncoming(elseValue, elseBlock);
        return phi;
    }

    if (!thenBlock->getTerminator()) {
        builder.SetInsertPoint(thenBlock);
        builder.CreateBr(mergeBlock);
    }
    if (!hasElseValue && !elseBlock->getTerminator()) {
        builder.SetInsertPoint(elseBlock);
        builder.CreateBr(mergeBlock);
    }
    builder.SetInsertPoint(mergeBlock);
    return nullptr;
}
}  // namespace

ReturnType Visitor::operator()(const ast::IfElse& ifElse) const {
    auto loadIfRValue = [rvalue = sema::GetValueCategory{}(ifElse) ==
                                  sema::ValueCategory::RValue,
                         this](ReturnType&& val) -> ReturnType {
        if (auto err = val.takeError())
            return std::move(err);
        return rvalue ? this->load(std::move(val)) : std::move(val);
    };
    DECLARE_OR_RETURN_NOVOID(cond, load(ifElse.cond->visit(*this)));
    if (cond->getType() != Type::get_type(Type::ID::Bool,
                                          instance.impl->context,
                                          instance.impl->typeTable)) {
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
    (void)branchInst;
    builder.SetInsertPoint(thenBlock);
    DECLARE_OR_RETURN(thenBranch,
                      loadIfRValue(ifElse.thenBranch->visit(*this)));
    auto* const thenBlockEnd = builder.GetInsertBlock();

    func->getBasicBlockList().push_back(elseBlock);
    builder.SetInsertPoint(elseBlock);
    const bool hasElseBranch = ifElse.elseBranch != nullptr;
    if (!hasElseBranch && !isVoid(thenBranch)) {
        return err("'if' without 'else' must evaluate to void");
    }
    DECLARE_OR_RETURN(elseBranch,
                      hasElseBranch
                            ? loadIfRValue(ifElse.elseBranch->visit(*this))
                            : ReturnType{nullptr});
    auto* const elseBlockEnd = builder.GetInsertBlock();
    func->getBasicBlockList().push_back(mergeBlock);

    return setUpMergeBlock(thenBranch, thenBlockEnd, elseBranch, elseBlockEnd,
                           mergeBlock, builder);
}

ReturnType Visitor::operator()(const ast::While& while_) const {
    auto& builder = instance.impl->builder;

    instance.impl->symtable.pushScope();
    util::ScopeGuard pop{[&] { instance.impl->symtable.popScope(); }};

    auto* const func = builder.GetInsertBlock()->getParent();
    auto* const loopInitBlock =
          llvm::BasicBlock::Create(instance.impl->context, "loop_init", func);
    builder.CreateBr(loopInitBlock);
    builder.SetInsertPoint(loopInitBlock);
    if (while_.init) {
        DECLARE_OR_RETURN(init, while_.init->visit(*this));
        (void)init;
    }

    auto* const loopCondBlock =
          llvm::BasicBlock::Create(instance.impl->context, "loop_cond");
    auto* const loopBodyBlock =
          llvm::BasicBlock::Create(instance.impl->context, "loop_body");
    auto* const loopMergeBlock =
          llvm::BasicBlock::Create(instance.impl->context, "loop_merge");
    func->getBasicBlockList().push_back(loopCondBlock);
    builder.CreateBr(loopCondBlock);
    builder.SetInsertPoint(loopCondBlock);
    DECLARE_OR_RETURN(cond, load(while_.cond->visit(*this)));
    builder.CreateCondBr(cond, loopBodyBlock, loopMergeBlock);

    func->getBasicBlockList().push_back(loopBodyBlock);
    builder.SetInsertPoint(loopBodyBlock);
    DECLARE_OR_RETURN(body, while_.body->visit(*this));
    (void)body;
    builder.CreateBr(loopCondBlock);

    func->getBasicBlockList().push_back(loopMergeBlock);
    builder.SetInsertPoint(loopMergeBlock);

    return nullptr;
}

ReturnType Visitor::operator()(const ast::LogicalAnd& a) const {
    DECLARE_OR_RETURN(lhs, load(a.lhs->visit(*this)));

    auto& builder = instance.impl->builder;
    auto* const func = builder.GetInsertBlock()->getParent();

    auto* const branchCond = [&] {
        if (!lhs) {
            // to have typechecked and still gotten this far, the lhs must have
            // been of type Never. so just branch on a dummy value
            return llvm::cantFail(this->operator()(ast::BoolLiteral{true}));
        }
        return lhs;
    }();
    auto* const lhsEnd = builder.GetInsertBlock();

    // short-circuit: if lhs is false, skip rhs
    auto* const rhsBlock =
          llvm::BasicBlock::Create(instance.impl->context, "and_rhs");
    auto* const mergeBlock =
          llvm::BasicBlock::Create(instance.impl->context, "and_merge");
    builder.CreateCondBr(branchCond, rhsBlock, mergeBlock);

    func->getBasicBlockList().push_back(rhsBlock);
    builder.SetInsertPoint(rhsBlock);
    DECLARE_OR_RETURN(rhs, load(a.rhs->visit(*this)));
    auto* const rhsEnd = builder.GetInsertBlock();
    builder.CreateBr(mergeBlock);

    func->getBasicBlockList().push_back(mergeBlock);
    builder.SetInsertPoint(mergeBlock);
    auto* const phi = builder.CreatePHI(
          Type::get_type(Type::ID::Bool, instance.impl->context,
                         instance.impl->typeTable),
          2, "and_phi");
    phi->addIncoming(branchCond, lhsEnd);
    phi->addIncoming(
          rhs ? rhs : llvm::cantFail(this->operator()(ast::BoolLiteral{true})),
          rhsEnd);
    return phi;
}
ReturnType Visitor::operator()(const ast::LogicalOr& o) const {
    DECLARE_OR_RETURN(lhs, load(o.lhs->visit(*this)));

    auto& builder = instance.impl->builder;
    auto* const func = builder.GetInsertBlock()->getParent();

    auto* const branchCond = [&] {
        if (!lhs) {
            // to have typechecked and still gotten this far, the lhs must have
            // been of type Never. so just branch on a dummy value
            return llvm::cantFail(this->operator()(ast::BoolLiteral{true}));
        }
        return lhs;
    }();
    auto* const lhsEnd = builder.GetInsertBlock();

    // short-circuit: if lhs is true, skip rhs
    auto* const rhsBlock =
          llvm::BasicBlock::Create(instance.impl->context, "or_rhs");
    auto* const mergeBlock =
          llvm::BasicBlock::Create(instance.impl->context, "or_merge");
    builder.CreateCondBr(branchCond, mergeBlock, rhsBlock);

    func->getBasicBlockList().push_back(rhsBlock);
    builder.SetInsertPoint(rhsBlock);
    DECLARE_OR_RETURN(rhs, load(o.rhs->visit(*this)));
    auto* const rhsEnd = builder.GetInsertBlock();
    builder.CreateBr(mergeBlock);

    func->getBasicBlockList().push_back(mergeBlock);
    builder.SetInsertPoint(mergeBlock);
    auto* const phi = builder.CreatePHI(
          Type::get_type(Type::ID::Bool, instance.impl->context,
                         instance.impl->typeTable),
          2, "or_phi");
    phi->addIncoming(branchCond, lhsEnd);
    phi->addIncoming(
          rhs ? rhs : llvm::cantFail(this->operator()(ast::BoolLiteral{true})),
          rhsEnd);
    return phi;
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
    DECLARE_OR_RETURN(init, load(decl.init->visit(*this)));
    if (!init) {
        // to have typechecked and still gotten this far, the initializer must
        // have been of type Never. so this declaration will never actually
        // happen
        auto* const type =
              Type::get_type(decl.type.has_value() ? *decl.type : Type::ID::Int,
                             instance.impl->context, instance.impl->typeTable);
        return llvm::UndefValue::get(type);

    } else if (init->getType()->isVoidTy()) {
        return err("variable initializer cannot have void type");
    }
    if (decl.type.has_value()) {
        if (const auto declT = Type::get_type(
                  *decl.type, instance.impl->context, instance.impl->typeTable);
            declT != init->getType()) {
            return err(
                  "type of initializer does not match variable's specified "
                  "type (" +
                  str(init->getType()) + " vs " + str(declT) + ")");
        }
    }
    auto* const t = init->getType();

    auto& builder = instance.impl->builder;
    auto* const alloca = createAllocaInEntryBlock(t, decl.name);
    instance.impl->symtable.insert(decl.name, alloca);

    auto* store = builder.CreateStore(init, alloca);

    return store;
}

ReturnType Visitor::operator()(const ast::Assignment& assign) const {
    DECLARE_OR_RETURN(dest, assign.dest->visit(*this));
    DECLARE_OR_RETURN(rhs, assign.rhs->visit(*this));
    if (!rhs) {
        // to have typechecked and still gotten this far, the rhs must
        // have been of type Never. so this assignment will never actually
        // happen
        return llvm::UndefValue::get(getLoadedType(dest));
    }
    auto& builder = instance.impl->builder;

    if (auto* const destType = getLoadedType(dest);
        destType != rhs->getType()) {
        return err("cannot assign new value, type mismatch (" + str(destType) +
                   " vs " + str(rhs->getType()) + ")");
    }
    builder.CreateStore(rhs, dest);
    return nullptr;
}

ReturnType Visitor::operator()(const ast::Return& ret) const {
    auto* const returnType = instance.impl->getCurrentReturnType();
    if (!ret.value) {
        if (!returnType->isVoidTy())
            return err("returning void from non-void function");
        return instance.impl->builder.CreateRetVoid();
    }
    DECLARE_OR_RETURN(value, load(ret.value->visit(*this)));
    if (returnType != value->getType()) {
        return err("return type mismatch");
    }
    return value->getType()->isVoidTy()
                 ? instance.impl->builder.CreateRetVoid()
                 : instance.impl->builder.CreateRet(value);
}

/////

llvm::Error Visitor::operator()(const ast::StructDef& sd) const {
    auto result = instance.impl->typeTable.createNewType(sd);
    if (auto err = result.takeError()) {
        return err;
    }
    std::vector<llvm::Type*> fields;
    fields.reserve(sd.fields.size());
    for (auto& field : sd.fields) {
        auto* const type = Type::get_type(field.type, instance.impl->context,
                                          instance.impl->typeTable);
        assert(type);
        fields.push_back(type);
    }
    result->get().type->setBody(fields, false);
    return llvm::Error::success();
}

ReturnType Visitor::operator()(const ast::StructValue& v) const {
    auto* const alloca = createAllocaInEntryBlock(
          Type::get_type(v.type, instance.impl->context,
                         instance.impl->typeTable),
          llvm::Twine("temp_alloca_") +
                Type::to_string(v.type, &instance.impl->typeTable));
    return alloca;
}

ReturnType Visitor::operator()(const ast::StructMemberAccess& m) const {
    auto& builder = instance.impl->builder;

    DECLARE_OR_RETURN(lhs, m.lhs->visit(*this));
    auto* const loadType = getLoadedType(lhs);
    assert(llvm::isa<llvm::StructType>(loadType));

    auto* const ptr = [&]() -> llvm::Value* {
        if (!llvm::isa<llvm::PointerType>(lhs->getType())) {
            auto* const alloca =
                  createAllocaInEntryBlock(loadType, "temp_alloca");
            builder.CreateStore(lhs, alloca);
            return alloca;
        }
        return lhs;
    }();
    assert(llvm::isa<llvm::PointerType>(ptr->getType()));

    const auto record =
          instance.impl->typeTable.get(llvm::cast<llvm::StructType>(loadType));
    assert(record);
    const auto idx = record->get().fields.indexOf(m.rhs);
    assert(idx);
    auto* const indexVal = cantFail(this->operator()(ast::IntLiteral{*idx}));
    auto* const zero = cantFail(this->operator()(ast::IntLiteral{0}));
    const std::array indices{zero, indexVal};
    auto* const gep = builder.CreateGEP(ptr, indices, lhs->getName() + "_gep");

    return gep;
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
    auto* const t = Type::get_type(type, instance.impl->context,
                                   instance.impl->typeTable);
    assert(t);
    assert(t->isFloatingPointTy());
    return llvm::ConstantFP::get(t, value);
}
Value* Visitor::make_integer_constant(Type::ID type,
                                      const llvm::APInt& value) const {
    auto* const t = Type::get_type(type, instance.impl->context,
                                   instance.impl->typeTable);
    assert(t);
    assert(t->isIntegerTy());
    return llvm::ConstantInt::get(t, value);
}

namespace {
bool needsLoad(llvm::Value* val) {
    if (llvm::isa<llvm::AllocaInst>(val) ||
        llvm::isa<llvm::GetElementPtrInst>(val))
        return true;
    if (auto* const phi = llvm::dyn_cast<llvm::PHINode>(val)) {
        return std::all_of(phi->incoming_values().begin(),
                           phi->incoming_values().end(), needsLoad);
    }
    return false;
}
}  // namespace
llvm::Value* Visitor::load(llvm::Value* val) const {
    if (val && needsLoad(val)) {
        return instance.impl->builder.CreateLoad(val, val->getName() + "_load");
    }
    return val;
}
ReturnType Visitor::load(ReturnType&& ret) const {
    if (auto err = ret.takeError())
        return std::move(err);
    return load(*ret);
}
llvm::Type* Visitor::getLoadedType(llvm::Value* val) {
    assert(val);
    if (llvm::isa<llvm::AllocaInst>(val) ||
        llvm::isa<llvm::GetElementPtrInst>(val)) {
        return llvm::cast<llvm::PointerType>(val->getType())->getElementType();
    }
    if (auto* const phi = llvm::dyn_cast<llvm::PHINode>(val)) {
        assert(phi->getNumIncomingValues() > 0);
        return getLoadedType(*phi->incoming_values().begin());
    }
    return val->getType();
}

}  // namespace codegen
