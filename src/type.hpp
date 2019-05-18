#ifndef CERAPH_TYPE_HPP
#define CERAPH_TYPE_HPP

#include "llvm/IR/Type.h"

namespace llvm {
class LLVMContext;
}

namespace Type {

enum class ID {
    Int,
    Float,
    Double,
    StringLiteral,
};

template <typename OStream>
OStream& operator<<(OStream& ostr, ID ty) {
    switch (ty) {
        case ID::Int:
            ostr << "int";
            break;
        case ID::Float:
            ostr << "float";
            break;
        case ID::Double:
            ostr << "double";
            break;
        case ID::StringLiteral:
            ostr << "string";
            break;
    }
    return ostr;
}

template <ID Ty>
struct is_floating : std::bool_constant<Ty == ID::Float || Ty == ID::Double> {};
template <ID Ty>
inline constexpr bool is_floating_v = is_floating<Ty>::value;

template <ID Ty>
struct is_integer : std::bool_constant<Ty == ID::Int> {};
template <ID Ty>
inline constexpr bool is_integer_v = is_integer<Ty>::value;

template <ID Ty>
struct num_bits;
template <>
struct num_bits<ID::Int> : std::integral_constant<std::size_t, 32> {};
template <ID Ty>
struct is_signed;
template <>
struct is_signed<ID::Int> : std::true_type {};

llvm::Type* get_type(ID theType, llvm::LLVMContext& context);

}  // namespace Type

#endif
