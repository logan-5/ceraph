#ifndef CERAPH_TYPE_HPP
#define CERAPH_TYPE_HPP

#include "llvm/ADT/StringRef.h"
#include "llvm/IR/DerivedTypes.h"
#include "llvm/IR/Type.h"

#include <optional>

namespace llvm {
class LLVMContext;
}

namespace ast {
class FunctionProto;
}

namespace Type {

enum class ID {
    Never,

    Bool,
    Int,
    Float,
    Double,
    StringLiteral,
    Void,
};

std::optional<ID> from_name(llvm::StringRef name);

const char* to_string(ID ty);
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

llvm::Type* get_type(ID theType, llvm::LLVMContext& context);

llvm::FunctionType* get_type(const ast::FunctionProto& proto,
                             llvm::LLVMContext& context);

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

}  // namespace Type

#endif
