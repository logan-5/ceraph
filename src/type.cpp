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

std::optional<Type::ID> StructFields::typeOf(llvm::StringRef name) const {
    const auto it = this->find(name);
    return it == fields.end() ? std::nullopt : std::optional{it->type};
}

}  // namespace Type
