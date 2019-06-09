#include "type.hpp"

#include "ast.hpp"
#include "ceraph_fwd.hpp"
#include "util.hpp"

#include "llvm/ADT/StringMap.h"
#include "llvm/IR/DerivedTypes.h"

#include <range/v3/view/transform.hpp>

namespace Type {

std::optional<ID> from_name(llvm::StringRef name) {
    static const llvm::StringMap<ID> typeNames{
          {"bool", ID::Bool},     {"int", ID::Int},   {"float", ID::Float},
          {"double", ID::Double}, {"void", ID::Void},
    };
    if (auto typeIt = typeNames.find(name); typeIt != typeNames.end()) {
        return typeIt->getValue();
    }
    return std::nullopt;
}

llvm::Type* get_type(ID theType,
                     llvm::LLVMContext& theContext,
                     const UserDefinedTypeTable& utt) {
    switch (theType) {
        case ID::Never:
            return nullptr;

        case ID::Null:
            return llvm::PointerType::get(llvm::IntegerType::get(theContext, 8),
                                          0);

        case ID::Bool:
            return llvm::IntegerType::get(theContext,
                                          num_bits<ID::Bool>::value);
        case ID::Int:
            return llvm::IntegerType::get(theContext, num_bits<ID::Int>::value);
        case ID::Float:
            return llvm::Type::getFloatTy(theContext);
        case ID::Double:
            return llvm::Type::getDoubleTy(theContext);
        case ID::StringLiteral:
            assert(false && "I dunno how to implement this yet. I'm sorry");
            break;
        case ID::Void:
            return llvm::Type::getVoidTy(theContext);

        default: {
            if (is_user_defined(theType)) {
                if (const auto record = utt.get(theType))
                    return record->get().type;
                assert(false && "unknown type");
                return nullptr;
            }
        }
    }
    assert(false && "unreachable");
    return nullptr;
}

llvm::FunctionType* get_type(const ast::FunctionProto& proto,
                             llvm::LLVMContext& theContext,
                             const UserDefinedTypeTable& utt) {
    auto* const returnType = get_type(proto.returnType, theContext, utt);
    assert(llvm::FunctionType::isValidReturnType(returnType));

    const auto argTypes =
          rv::transform(
                proto.args,
                [&](const ast::FunctionProto::Arg& arg) {
                    auto* const theType = get_type(arg.type, theContext, utt);
                    assert(llvm::FunctionType::isValidArgumentType(theType));
                    return theType;
                }) |
          ranges::to_vector;

    return llvm::FunctionType::get(returnType, argTypes, false);
}

std::string to_string(ID ty, const UserDefinedTypeTable* utt) {
    switch (ty) {
        case ID::Never:
            return "never";
        case ID::Null:
            return "null";
        case ID::Bool:
            return "bool";
        case ID::Int:
            return "int";
        case ID::Float:
            return "float";
        case ID::Double:
            return "double";
        case ID::StringLiteral:
            return "string";
        case ID::Void:
            return "void";

        default: {
            if (is_user_defined(ty)) {
                if (utt) {
                    if (const auto name = utt->get_name(ty); name.has_value())
                        return *name;
                    assert(false && "unknown type");
                    return "unknown";
                }
                return "[user defined type]";
            }
        }
    }
    assert(false && "unreachable");
    return "";
}

/////////////////
// compound types

struct ToStringVisitor {
    using ReturnType = llvm::Twine;

    const UserDefinedTypeTable* table;

    ReturnType operator()(const ID ty) const { return to_string(ty, table); }
    ReturnType operator()(const Pointer& ptr) const {
        return ReturnType{"pointer to "} + std::visit(*this, *ptr.to);
    }
    ReturnType operator()(const Array& arr) const {
        return ReturnType{"array of "} + std::visit(*this, *arr.of) +
               ", size " + ReturnType{arr.size};
    }
};

std::string to_string(const CompoundType& ty,
                      const UserDefinedTypeTable* table) {
    return std::visit(ToStringVisitor{table}, ty).str();
}

struct GetLLVMTypeVisitor {
    using ReturnType = llvm::Type*;

    std::reference_wrapper<llvm::LLVMContext> context;
    std::reference_wrapper<const UserDefinedTypeTable> utt;

    ReturnType operator()(const ID ty) const {
        return get_type(ty, context, utt);
    }
    ReturnType operator()(const Pointer& ptr) const {
        const auto addressSpace = 0;  // i do not know
        return llvm::PointerType::get(std::visit(*this, *ptr.to), addressSpace);
    }
    ReturnType operator()(const Array& arr) const {
        return llvm::ArrayType::get(std::visit(*this, *arr.of), arr.size);
    }
};

llvm::Type* get_type(const CompoundType& theType,
                     llvm::LLVMContext& context,
                     const UserDefinedTypeTable& utt) {
    return std::visit(GetLLVMTypeVisitor{context, utt}, theType);
}

struct CompoundMatched {
    using ReturnType = std::optional<CompoundType>;

    ReturnType operator()(const ID a, const ID b) const {
        return matched(a, b);
    }
    ReturnType operator()(const Pointer& a, const Pointer& b) const {
        // TODO void* and stuff?
        if (auto inner = std::visit(*this, *a.to, *b.to))
            return Pointer{ptr(std::move(*inner))};
        return std::nullopt;
    }
    ReturnType operator()(const Array& a, const Array& b) const {
        if (a.size == b.size) {
            if (auto elem = std::visit(*this, *a.of, *b.of))
                return Array{ptr(std::move(*elem)), a.size};
        }
        return std::nullopt;
    }

    // mismatch
    template <typename T, typename U>
    ReturnType operator()(const T&, const U&) const {
        return std::nullopt;
    }
};

std::optional<CompoundType> matched(const CompoundType& a,
                                    const CompoundType& b) {
    return std::visit(CompoundMatched{}, a, b);
}

//////////////////////////
// user-defined type table

char UserDefinedTypeTable::DuplicateTypeError::ID = 3;

auto UserDefinedTypeTable::createNewType(const ast::StructDef& def)
      -> llvm::Expected<std::reference_wrapper<const TypeRecord>> {
    const auto& name = def.name;
    if (ids.find(name) != ids.end()) {
        return llvm::make_error<DuplicateTypeError>(
              llvm::Twine("redeclaration of type '") + name + "'");
    }
    auto* const theType = llvm::StructType::create(context, name);
    const auto theID = static_cast<ID>(
          names.size() + static_cast<std::size_t>(ID::UserDefinedMin));
    auto theRecord = TypeRecord{theID, def, theType};
    names.push_back(name);
    llvmNames[theType] = name;
    const auto [it, _] = ids.insert(std::pair{name, std::move(theRecord)});
    return std::cref(it->getValue());
}

auto UserDefinedTypeTable::get(llvm::StringRef name) const
      -> std::optional<std::reference_wrapper<const TypeRecord>> {
    if (auto it = ids.find(name); it != ids.end())
        return std::ref(it->getValue());
    return std::nullopt;
}
std::optional<std::reference_wrapper<const std::string>>
UserDefinedTypeTable::get_name(Type::ID id_) const {
    const auto idx = static_cast<std::size_t>(id_) -
                     static_cast<std::size_t>(ID::UserDefinedMin);
    if (idx >= names.size())
        return std::nullopt;
    return names[idx];
}
std::optional<std::reference_wrapper<const std::string>>
UserDefinedTypeTable::get_name(llvm::StructType* type) const {
    assert(type);
    if (auto it = llvmNames.find(type); it != llvmNames.end())
        return it->second;
    return std::nullopt;
}

auto UserDefinedTypeTable::get(Type::ID id_) const
      -> std::optional<std::reference_wrapper<const TypeRecord>> {
    if (const auto name = get_name(id_); name.has_value()) {
        return get(name->get());
    }
    return std::nullopt;
}
auto UserDefinedTypeTable::get(llvm::StructType* type) const
      -> std::optional<std::reference_wrapper<const TypeRecord>> {
    if (const auto name = get_name(type); name.has_value()) {
        return get(name->get());
    }
    return std::nullopt;
}

namespace {
std::vector<StructFields::Field> makeFields(
      const ast::StructDef::Fields& astFields) {
    assert(!util::has_duplicates(astFields, &ast::StructDef::Field::name));

    std::vector<StructFields::Field> ret;
    ret.reserve(astFields.size());
    const auto begin = astFields.begin(), end = astFields.end();
    for (auto it = begin; it != end; ++it) {
        ret.emplace_back(it->name,
                         static_cast<std::int32_t>(std::distance(begin, it)),
                         it->type);
    }
    std::sort(ret.begin(), ret.end(),
              [](const StructFields::Field& a, const StructFields::Field& b) {
                  return a.name < b.name;
              });
    return ret;
}
}  // namespace

StructFields::StructFields(const ast::StructDef& def)
    : fields{makeFields(def.fields)} {}

auto StructFields::find(llvm::StringRef name) const -> Fields::const_iterator {
    return util::binary_find(fields.begin(), fields.end(), name, &Field::name);
}

std::optional<std::int32_t> StructFields::indexOf(llvm::StringRef name) const {
    const auto it = this->find(name);
    return it == fields.end() ? std::nullopt : std::optional{it->idx};
}

std::optional<Type::CompoundType> StructFields::typeOf(
      llvm::StringRef name) const {
    const auto it = this->find(name);
    return it == fields.end() ? std::nullopt : std::optional{it->type};
}

}  // namespace Type
