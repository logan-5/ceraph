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

template <typename OStream>
OStream& operator<<(OStream& ostr, Binary b) {
    switch (b) {
        case Binary::Plus:
            ostr << "Plus";
            break;
        case Binary::Minus:
            ostr << "Minus";
            break;
        case Binary::Multiply:
            ostr << "Multiply";
            break;
        case Binary::Divide:
            ostr << "Divide";
            break;
        case Binary::Modulo:
            ostr << "Modulo";
            break;

        case Binary::Less:
            ostr << "Less";
            break;
        case Binary::Equality:
            ostr << "Equality";
            break;
    }
    return ostr;
}

enum class Unary {
    Minus,
};

template <typename OStream>
OStream& operator<<(OStream& ostr, Unary u) {
    switch (u) {
        case Unary::Minus:
            ostr << "Unary Minus";
    }
    return ostr;
}

}  // namespace Operator

#endif
