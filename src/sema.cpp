#include "sema.hpp"

#include "util.hpp"

using llvm::Twine;

namespace sema {
#define DECLARE_OR_RETURN(NAME, INIT)       \
    auto NAME##OrErr = INIT;                \
    if (auto err = NAME##OrErr.takeError()) \
        return std::move(err);              \
    const auto& NAME = *NAME##OrErr
#define UNWRAP_OR_RETURN(NAME, INIT, MSG) \
    auto NAME##Opt = INIT;                \
    if (!NAME##Opt.has_value())           \
        return err(MSG);                  \
    const auto& NAME = *NAME##Opt;

auto err(const llvm::Twine& desc) {
    return llvm::make_error<TypeCheckError>(desc);
}

char TypeCheckError::ID = 1;

void TypeResultTable::insert(ast::Node k, Type::CompoundType v) {
    assert(table.find(k) == table.end());
    table.emplace(std::move(k), std::move(v));
}
const Type::CompoundType& TypeResultTable::get(const ast::Node& k) const {
    auto it = table.find(k);
    assert(it != table.end());
    return it->second;
}

auto GetType::ret(const std::optional<Type::CompoundType> ty,
                  const llvm::Twine& err) const -> ReturnType {
    if (ty)
        return *ty;
    else
        return llvm::make_error<TypeCheckError>(err);
}

auto GetType::operator()(const ast::ArrayLiteral& arr) const -> ReturnType {
    if (arr.elems.empty()) {
        return err("cannot infer type of zero-element array");
    }
    DECLARE_OR_RETURN(firstType, arr.elems[0]->visit(*this));
    for (auto it = std::next(arr.elems.begin()); it != arr.elems.end(); ++it) {
        DECLARE_OR_RETURN(elemType, (*it)->visit(*this));
        if (!Type::matched(firstType, elemType)) {
            return err(Twine("type mismatch in array literal elements ('") +
                       Type::to_string(firstType) + "' vs. '" +
                       Type::to_string(elemType) + "'");
        }
    }
    const auto type = Type::Array{firstType, arr.elems.size()};
    resultTable.get().insert(arr, type);
    return type;
}

auto GetType::operator()(const ast::Identifier& ident) const -> ReturnType {
    return ret(symbols->get(ident.name),
               llvm::Twine{"identifier '"} + ident.name + "' not found");
}

auto GetType::operator()(const ast::UnaryExpr& unary) const -> ReturnType {
    DECLARE_OR_RETURN(operand, unary.operand->visit(*this));
    using namespace Operator;
    switch (unary.op) {
        case Unary::Minus:
            if (Type::is_arithmetic(operand)) {
                return operand;
            } else {
                return err(Twine("cannot apply ") + to_string(unary.op) +
                           " to '" + to_string(operand) + "'");
            }
    }
}
auto GetType::operator()(const ast::BinaryExpr& binary) const -> ReturnType {
    DECLARE_OR_RETURN(lhs, binary.lhs->visit(*this));
    DECLARE_OR_RETURN(rhs, binary.rhs->visit(*this));

    UNWRAP_OR_RETURN(t, Type::matched(lhs, rhs),
                     Twine("incompatible types for ") + to_string(binary.op));
    using namespace Operator;
    switch (binary.op) {
        case Binary::Plus:
        case Binary::Minus:
        case Binary::Multiply:
        case Binary::Divide:
        case Binary::Modulo:
            if (Type::is_arithmetic(t)) {
                return t;
            } else {
                return err(Twine("cannot apply ") + to_string(binary.op) +
                           " to '" + to_string(lhs) + "' and '" +
                           to_string(rhs) + "'");
            }
        case Binary::Less:
        case Binary::Equality:
            return Type::ID::Bool;
    }
}
auto GetType::operator()(const ast::FunctionProto& proto) const -> ReturnType {
    if (auto existing = symbols->get(proto.name);
        existing && !Type::matched(proto.returnType, *existing)) {
        return err(Twine("function '") + proto.name +
                   "' redefined with different return type ('" +
                   to_string(proto.returnType) + "' vs '" +
                   to_string(*existing) + "')");
    }
    symbols->insertOrOverwrite(proto.name, proto.returnType);
    return Type::ID::Never;
}
auto GetType::operator()(const ast::FunctionDef& func) const -> ReturnType {
    if (auto existing = symbols->get(func.proto.name);
        existing && !Type::matched(func.proto.returnType, *existing)) {
        return err(Twine("function '") + func.proto.name +
                   "' redefined with different return type ('" +
                   to_string(func.proto.returnType) + "' vs '" +
                   to_string(*existing) + "')");
    }
    symbols->insertOrOverwrite(func.proto.name, func.proto.returnType);

    symbols->pushScope();
    util::ScopeGuard pop{[&] { symbols->popScope(); }};
    for (auto& arg : func.proto.args) {
        if (arg.name.has_value()) {
            symbols->insert(*arg.name, arg.type);
        }
    }

    DECLARE_OR_RETURN(body, func.body->visit(*this));
    (void)body;
    return Type::ID::Never;
}
auto GetType::operator()(const ast::FunctionCall& call) const -> ReturnType {
    if (auto retType = symbols->get(call.name)) {
        return *retType;
    }
    return err(Twine("call to unknown identifier '") + call.name + "'");
}

auto GetType::operator()(const ast::IfElse& ifElse) const -> ReturnType {
    DECLARE_OR_RETURN(cond, ifElse.cond->visit(*this));
    if (!Type::matched(cond, Type::ID::Bool).has_value()) {
        return err(Twine("'if' condition not a boolean expression, found '") +
                   to_string(cond) + "' instead");
    }
    DECLARE_OR_RETURN(then, ifElse.thenBranch->visit(*this));
    if (!ifElse.elseBranch &&
        !Type::matched(then, Type::ID::Void).has_value()) {
        return err(Twine("'if' without 'else' must evaluate to ") +
                   to_string(Type::ID::Void));
    } else if (!ifElse.elseBranch) {
        return Type::ID::Void;
    }
    DECLARE_OR_RETURN(else_, ifElse.elseBranch->visit(*this));
    return ret(Type::matched(then, else_),
               "mismatched types in if/else branches");
}

auto GetType::operator()(const ast::While& while_) const -> ReturnType {
    symbols->pushScope();
    util::ScopeGuard pop{[&] { symbols->popScope(); }};
    if (while_.init) {
        DECLARE_OR_RETURN(init, while_.init->visit(*this));
        (void)init;
    }
    DECLARE_OR_RETURN(cond, while_.cond->visit(*this));
    if (!Type::matched(cond, Type::ID::Bool).has_value()) {
        return err(
              Twine("'while' condition not a boolean expression, found '") +
              to_string(cond) + "' instead");
    }
    DECLARE_OR_RETURN(body, while_.body->visit(*this));
    (void)body;
    return Type::ID::Void;
}
auto GetType::operator()(const ast::LogicalAnd& a) const -> ReturnType {
    DECLARE_OR_RETURN(lhs, a.lhs->visit(*this));
    if (!Type::matched(lhs, Type::ID::Bool).has_value()) {
        return err(Twine("left-hand side of logical-and not a boolean "
                         "expression, found '") +
                   to_string(lhs) + "' instead");
    }
    DECLARE_OR_RETURN(rhs, a.lhs->visit(*this));
    if (!Type::matched(rhs, Type::ID::Bool).has_value()) {
        return err(Twine("right-hand side of logical-and not a boolean "
                         "expression, found '") +
                   to_string(rhs) + "' instead");
    }
    return Type::ID::Bool;
}
auto GetType::operator()(const ast::LogicalOr& o) const -> ReturnType {
    DECLARE_OR_RETURN(lhs, o.lhs->visit(*this));
    if (!Type::matched(lhs, Type::ID::Bool).has_value()) {
        return err(Twine("left-hand side of logical-or not a boolean "
                         "expression, found '") +
                   to_string(lhs) + "' instead");
    }
    DECLARE_OR_RETURN(rhs, o.lhs->visit(*this));
    if (!Type::matched(rhs, Type::ID::Bool).has_value()) {
        return err(Twine("right-hand side of logical-or not a boolean "
                         "expression, found '") +
                   to_string(rhs) + "' instead");
    }
    return Type::ID::Bool;
}

auto GetType::operator()(const ast::NullStmt) const -> ReturnType {
    return Type::ID::Void;
}
auto GetType::operator()(const ast::Block& block) const -> ReturnType {
    if (block.stmts.empty())
        return Type::ID::Void;

    symbols->pushScope();
    util::ScopeGuard pop{[&] { symbols->popScope(); }};

    const auto lastStatementIt = std::prev(block.stmts.end());
    for (auto it = block.stmts.begin(); it != lastStatementIt; ++it) {
        DECLARE_OR_RETURN(stmt, (*it)->visit(*this));
        if (Type::is_never(stmt))
            return stmt;
    }

    return (*lastStatementIt)->visit(*this);
}

auto GetType::operator()(const ast::Declaration& decl) const -> ReturnType {
    DECLARE_OR_RETURN(rhs, decl.init->visit(*this));
    if (decl.type.has_value() && !Type::matched(*decl.type, rhs)) {
        return err(Twine("initializer has different type than variable type "
                         "given ('") +
                   to_string(*decl.type) + "' vs. '" + to_string(rhs) + "'");
    }
    symbols->insert(decl.name, rhs);
    return Type::ID::Void;
}
auto GetType::operator()(const ast::Assignment& assign) const -> ReturnType {
    if (assign.dest->visit(GetValueCategory{}) != ValueCategory::LValue) {
        return err("value not assignable");
    }
    DECLARE_OR_RETURN(dest, assign.dest->visit(*this));
    DECLARE_OR_RETURN(rhs, assign.rhs->visit(*this));
    if (Type::matched(dest, rhs).has_value()) {
        return Type::ID::Void;
    }
    return err(Twine("mismatched types in assignment: '") + to_string(dest) +
               "' and '" + to_string(rhs) + "'");
}

auto GetType::operator()(const ast::Return& ret) const -> ReturnType {
    if (ret.value) {
        DECLARE_OR_RETURN(value, ret.value->visit(*this));
        (void)value;
    }
    return Type::ID::Never;
}

auto GetType::operator()(const ast::StructValue& val) const -> ReturnType {
    return val.type;
}

auto GetType::operator()(const ast::StructMemberAccess& m) const -> ReturnType {
    DECLARE_OR_RETURN(lhsOrPtr, m.lhs->visit(*this));
    DECLARE_OR_RETURN(lhs, [&]() -> ReturnType {
        if (m.dereference) {
            if (auto pointee = Type::get_pointee(lhsOrPtr);
                pointee.has_value()) {
                return *pointee;
            } else {
                return err(Twine("cannot reference object of type '") +
                           Type::to_string(lhsOrPtr) + "')");
            }
        }
        return lhsOrPtr;
    }());
    if (!Type::is_user_defined(lhs)) {
        return err(Twine("cannot do member access on a scalar type ('") +
                   Type::to_string(lhs, &typeTable.get()) + "')");
    }
    const auto record = typeTable.get().get(lhs);
    assert(record);
    const auto fieldType = record->get().fields.typeOf(m.rhs);
    if (!fieldType) {
        return err(Twine("no member named '") + m.rhs + "' in type '" +
                   Type::to_string(lhs, &typeTable.get()) + "'");
    }
    return *fieldType;
}

auto GetType::operator()(const ast::ExplicitCast& cast) const -> ReturnType {
    DECLARE_OR_RETURN(operand, cast.operand->visit(*this));
    if (!Type::is_valid_cast(operand, cast.toType)) {
        return err(Twine("cannot cast '") +
                   Type::to_string(operand, &typeTable.get()) + "' to '" +
                   Type::to_string(cast.toType, &typeTable.get()) + "'");
    }
    return cast.toType;
}

auto GetType::operator()(const ast::AddressOf& addr) const -> ReturnType {
    DECLARE_OR_RETURN(operandT, addr.operand->visit(*this));
    if (addr.operand->visit(sema::GetValueCategory{}) ==
        sema::ValueCategory::RValue) {
        return err("cannot take address of an rvalue");
    }
    return Type::Pointer{operandT};
}
auto GetType::operator()(const ast::Dereference& deref) const -> ReturnType {
    DECLARE_OR_RETURN(operand, deref.operand->visit(*this));
    if (!Type::is_pointer(operand)) {
        return err(Twine("cannot dereference object of type '") +
                   Type::to_string(operand, &typeTable.get()) + "'");
    }
    return *std::get<Type::Pointer>(operand).to;
}
auto GetType::operator()(const ast::Subscript& ss) const -> ReturnType {
    DECLARE_OR_RETURN(lhs, ss.lhs->visit(*this));
    if (!Type::is_array(lhs)) {
        return err(Twine("cannot subscript object of type '") +
                   Type::to_string(lhs, &typeTable.get()) + "'");
    }
    DECLARE_OR_RETURN(rhs, ss.rhs->visit(*this));
    if (!Type::is_integer(rhs)) {
        return err(Twine("array subscript must be an integer type (found '") +
                   Type::to_string(rhs, &typeTable.get()) + "')");
    }
    return *std::get<Type::Array>(lhs).of;
}

///////////////////////////////////////

ValueCategory GetValueCategory::operator()(const ast::ArrayLiteral&) const {
    return ValueCategory::RValue;
}

ValueCategory GetValueCategory::operator()(const ast::Identifier& ident) const {
    return ValueCategory::LValue;
}

ValueCategory GetValueCategory::operator()(const ast::UnaryExpr& unary) const {
    return ValueCategory::RValue;
}
ValueCategory GetValueCategory::operator()(
      const ast::BinaryExpr& binary) const {
    return ValueCategory::RValue;
}
ValueCategory GetValueCategory::operator()(
      const ast::FunctionProto& proto) const {
    return ValueCategory::None;
}
ValueCategory GetValueCategory::operator()(const ast::FunctionDef& func) const {
    return ValueCategory::None;
}
ValueCategory GetValueCategory::operator()(
      const ast::FunctionCall& call) const {
    return ValueCategory::RValue;
}

ValueCategory GetValueCategory::operator()(const ast::IfElse& ifElse) const {
    if (!ifElse.elseBranch)
        return ValueCategory::RValue;
    const auto then = ifElse.thenBranch->visit(*this);
    const auto else_ = ifElse.elseBranch->visit(*this);
    return (then == ValueCategory::LValue && else_ == ValueCategory::LValue)
                 ? ValueCategory::LValue
                 : ValueCategory::RValue;
}
ValueCategory GetValueCategory::operator()(const ast::While& while_) const {
    return ValueCategory::RValue;
}

ValueCategory GetValueCategory::operator()(const ast::LogicalAnd& a) const {
    return ValueCategory::RValue;
}
ValueCategory GetValueCategory::operator()(const ast::LogicalOr& o) const {
    return ValueCategory::RValue;
}

ValueCategory GetValueCategory::operator()(const ast::NullStmt nullStmt) const {
    return ValueCategory::None;
}
ValueCategory GetValueCategory::operator()(const ast::Block& block) const {
    if (block.stmts.empty())
        return ValueCategory::None;
    return block.stmts.back()->visit(*this);
}

ValueCategory GetValueCategory::operator()(const ast::Declaration& decl) const {
    return ValueCategory::None;
}
ValueCategory GetValueCategory::operator()(
      const ast::Assignment& assign) const {
    return ValueCategory::None;
}

ValueCategory GetValueCategory::operator()(const ast::Return& ret) const {
    return ValueCategory::RValue;
}

ValueCategory GetValueCategory::operator()(const ast::StructValue& v) const {
    return ValueCategory::RValue;
}
ValueCategory GetValueCategory::operator()(
      const ast::StructMemberAccess& m) const {
    const auto structCategory = m.lhs->visit(*this);
    return structCategory == ValueCategory::LValue ? ValueCategory::LValue
                                                   : ValueCategory::RValue;
}

ValueCategory GetValueCategory::operator()(
      const ast::ExplicitCast& cast) const {
    return ValueCategory::RValue;
}

ValueCategory GetValueCategory::operator()(const ast::AddressOf& addr) const {
    return ValueCategory::RValue;
}
ValueCategory GetValueCategory::operator()(
      const ast::Dereference& deref) const {
    return ValueCategory::LValue;
}
ValueCategory GetValueCategory::operator()(const ast::Subscript& ss) const {
    return ss.lhs->visit(*this);
}

}  // namespace sema