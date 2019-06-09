#ifndef CERAPH_TYPE_HPP
#define CERAPH_TYPE_HPP

#include "llvm/ADT/StringRef.h"
#include "llvm/IR/DerivedTypes.h"
#include "llvm/IR/Type.h"
#include "llvm/Support/Error.h"

#include <map>
#include <memory>
#include <optional>
#include <string>
#include <variant>
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
    Null,

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

inline bool is_never(ID t) {
    return t == ID::Never;
}
inline bool is_null(ID t) {
    return t == ID::Null;
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

inline bool is_valid_cast(const ID from, const ID to) {
    return false;  // TODO
}

//////

struct CompoundType;
using InnerTypePtr = std::shared_ptr<CompoundType>;

struct Pointer {
    InnerTypePtr to;
};

struct Array {
    InnerTypePtr of;
    std::size_t size;
};

struct CompoundType : std::variant<ID, Pointer, Array> {
    using variant::variant;
};

inline InnerTypePtr ptr(CompoundType c) {
    return std::make_shared<CompoundType>(std::move(c));
}

std::string to_string(const CompoundType& ty,
                      const UserDefinedTypeTable* = nullptr);
template <typename OStream>
OStream& operator<<(OStream& ostr, const CompoundType& ty) {
    return ostr << to_string(ty);
}
llvm::Type* get_type(const CompoundType& theType,
                     llvm::LLVMContext& context,
                     const UserDefinedTypeTable& utt);

namespace detail {
inline constexpr bool unwrap_or_false(bool (*f)(ID), const CompoundType& cty) {
    const ID* const ty = std::get_if<ID>(&cty);
    return ty && f(*ty);
}
}  // namespace detail
inline constexpr bool is_floating(const CompoundType& ty) {
    return detail::unwrap_or_false(is_floating, ty);
}
inline constexpr bool is_integer(const CompoundType& ty) {
    return detail::unwrap_or_false(is_integer, ty);
}
inline constexpr bool is_arithmetic(const CompoundType& ty) {
    return detail::unwrap_or_false(is_arithmetic, ty);
}
inline constexpr bool is_user_defined(const CompoundType& ty) {
    return detail::unwrap_or_false(is_user_defined, ty);
}
inline bool is_void(const CompoundType& ty) {
    return detail::unwrap_or_false(is_void, ty);
}
inline bool is_never(const CompoundType& ty) {
    return detail::unwrap_or_false(is_never, ty);
}
inline bool is_null(const CompoundType& ty) {
    return detail::unwrap_or_false(is_null, ty);
}

std::optional<CompoundType> matched(const CompoundType& a,
                                    const CompoundType& b);

bool is_valid_cast(const CompoundType& from, const CompoundType& to);

//////

class StructFields {
   public:
    StructFields(const ast::StructDef& def);

    std::optional<std::int32_t> indexOf(llvm::StringRef name) const;
    std::optional<Type::CompoundType> typeOf(llvm::StringRef name) const;

    struct Field {
        explicit Field(std::string n, std::int32_t i, Type::CompoundType t)
            : name{std::move(n)}, idx{i}, type{std::move(t)} {}
        std::string name;
        std::int32_t idx;
        Type::CompoundType type;
    };

   private:
    using Fields = std::vector<Field>;
    Fields::const_iterator find(llvm::StringRef name) const;
    Fields fields;
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

    llvm::Expected<std::reference_wrapper<const TypeRecord>> createNewType(
          const ast::StructDef& def);

    std::optional<std::reference_wrapper<const TypeRecord>> get(
          llvm::StringRef name) const;
    std::optional<std::reference_wrapper<const TypeRecord>> get(
          Type::ID id_) const;
    std::optional<std::reference_wrapper<const TypeRecord>> get(
          Type::CompoundType cty) const {
        return unwrap_or_none([this](const Type::ID id_) { return get(id_); },
                              cty);
    }
    std::optional<std::reference_wrapper<const TypeRecord>> get(
          llvm::StructType* type) const;
    std::optional<std::reference_wrapper<const std::string>> get_name(
          Type::ID id_) const;
    std::optional<std::reference_wrapper<const std::string>> get_name(
          Type::CompoundType cty) const {
        return unwrap_or_none(
              [this](const Type::ID id_) { return get_name(id_); }, cty);
    }
    std::optional<std::reference_wrapper<const std::string>> get_name(
          llvm::StructType* type) const;

   private:
    template <typename MemFn>
    std::invoke_result_t<MemFn, Type::ID> unwrap_or_none(
          MemFn fn,
          const Type::CompoundType& cty) const {
        if (const ID* const ty = std::get_if<ID>(&cty))
            return std::invoke(fn, *ty);
        return std::nullopt;
    }

    std::reference_wrapper<llvm::LLVMContext> context;

    llvm::StringMap<TypeRecord> ids;
    std::vector<std::string> names;
    std::map<llvm::StructType*, std::string> llvmNames;
};

}  // namespace Type

#endif
