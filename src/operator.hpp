#ifndef CERAPH_OPERATOR_HPP
#define CERAPH_OPERATOR_HPP

namespace Operator {

enum class Binary {
    Plus,
    Minus,
    Multiply,
    Divide,
    Modulo,

    Less,
    Equality,
};

const char* to_string(Binary b);
template <typename OStream>
OStream& operator<<(OStream& ostr, Binary b) {
    return ostr << to_string(b);
}

enum class Unary {
    Minus,
};

const char* to_string(Unary u);
template <typename OStream>
OStream& operator<<(OStream& ostr, Unary u) {
    return ostr << to_string(u);
}

}  // namespace Operator

#endif
