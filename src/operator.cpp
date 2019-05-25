#include "operator.hpp"

namespace Operator {
const char* to_string(Binary b) {
    switch (b) {
        case Binary::Plus:
            return "Plus";
        case Binary::Minus:
            return "Minus";
        case Binary::Multiply:
            return "Multiply";
        case Binary::Divide:
            return "Divide";
        case Binary::Modulo:
            return "Modulo";

        case Binary::Less:
            return "Less";
        case Binary::Equality:
            return "Equality";
    }
    return "";
}

const char* to_string(Unary u) {
    switch (u) {
        case Unary::Minus:
            return "Unary Minus";
    }
}
}  // namespace Operator
