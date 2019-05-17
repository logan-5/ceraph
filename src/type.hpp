#ifndef CERAPH_TYPE_HPP
#define CERAPH_TYPE_HPP

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

}  // namespace Type

#endif
