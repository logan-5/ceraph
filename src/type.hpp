#ifndef CERAPH_TYPE_HPP
#define CERAPH_TYPE_HPP

#include "llvm/ADT/StringRef.h"
#include "llvm/IR/DerivedTypes.h"
#include "llvm/IR/Type.h"
#include "llvm/Support/Error.h"

#include <optional>
#include <string>
#include <vector>

namespace llvm {
class LLVMContext;
}  // namespace llvm

namespace ast {
struct FunctionProto;
struct StructDef;
}  // namespace ast

namespace Type {

class UserDefinedTypeTable;

enum class ID {
    Never,

    Bool,
    Int,
    Float,
    Double,
    StringLiteral,
    Void,

    UserDefinedMin,
};

std::optional<ID> from_name(llvm::StringRef name);

std::string to_string(ID ty, const UserDefinedTypeTable* = nullptr);
template <typename OStream>
OStream& operator<<(OStream& ostr, ID ty) {
    return ostr << to_string(ty);
}

inline constexpr bool is_floating(ID ty) {
    return ty == ID::Float || ty == ID::Double;
}

inline constexpr bool is_integer(ID ty) {
    return ty == ID::Int || ty == ID::Bool;
}

inline constexpr bool is_arithmetic(ID ty) {
    return is_floating(ty) || (is_integer(ty) && ty != ID::Bool);
}

inline constexpr bool is_user_defined(ID ty) {
    return ty >= ID::UserDefinedMin;
}

template <ID Ty>
struct num_bits;
template <>
struct num_bits<ID::Int> : std::integral_constant<std::size_t, 32> {};
template <>
struct num_bits<ID::Bool> : std::integral_constant<std::size_t, 1> {};
template <ID Ty>
struct is_signed;
template <>
struct is_signed<ID::Int> : std::true_type {};
template <>
struct is_signed<ID::Bool> : std::false_type {};

llvm::Type* get_type(ID theType,
                     llvm::LLVMContext& context,
                     const UserDefinedTypeTable& utt);

llvm::FunctionType* get_type(const ast::FunctionProto& proto,
                             llvm::LLVMContext& context,
                             const UserDefinedTypeTable& utt);

inline bool is_void(ID t) {
    return t == ID::Void;
}

inline std::optional<ID> matched(ID a, ID b) {
    if (a == ID::Never)
        return b;
    else if (b == ID::Never)
        return a;
    else if (a == b)
        return a;
    return std::nullopt;
}

class StructFields {
   public:
    StructFields(const ast::StructDef& def);

    std::int32_t indexOf(llvm::StringRef name);

    struct Field {
        explicit Field(std::string n, std::int32_t i)
            : name{std::move(n)}, idx{i} {}
        std::string name;
        std::int32_t idx;
    };

   private:
    std::vector<Field> fields;
};

class UserDefinedTypeTable {
   public:
    UserDefinedTypeTable(llvm::LLVMContext& c) : context{c} {}
    struct DuplicateTypeError : llvm::ErrorInfo<DuplicateTypeError> {
        static char ID;
        std::string description;

        DuplicateTypeError(const llvm::Twine& desc) : description{desc.str()} {}
        void log(llvm::raw_ostream& os) const override { os << description; }
        std::error_code convertToErrorCode() const override { return {}; }
    };
    struct TypeRecord {
        Type::ID typeId;
        Type::StructFields fields;
        llvm::StructType* type;
    };

    llvm::Expected<TypeRecord> createNewType(const ast::StructDef& def);

    std::optional<TypeRecord> get(llvm::StringRef name) const;
    llvm::StructType* get(Type::ID id_) const;
    std::optional<std::reference_wrapper<const std::string>> get_name(
          Type::ID id_) const;

   private:
    std::reference_wrapper<llvm::LLVMContext> context;

    llvm::StringMap<TypeRecord> ids;
    std::vector<std::string> names;
};

}  // namespace Type

#endif
