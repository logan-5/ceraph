#include "type.hpp"

#include "ast.hpp"
#include "util.hpp"

#include "llvm/ADT/StringMap.h"
#include "llvm/IR/DerivedTypes.h"

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

llvm::Type* get_type(ID theType, llvm::LLVMContext& theContext) {
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
    }
    assert(false);
    return nullptr;
}

llvm::FunctionType* get_type(const ast::FunctionProto& proto,
                             llvm::LLVMContext& theContext) {
    auto* const returnType = get_type(proto.returnType, theContext);
    assert(llvm::FunctionType::isValidReturnType(returnType));

    const auto argTypes =
          util::transform(proto.args, [&](const ast::FunctionProto::Arg& arg) {
              auto* const theType = get_type(arg.type, theContext);
              assert(llvm::FunctionType::isValidArgumentType(theType));
              return theType;
          });

    return llvm::FunctionType::get(returnType, argTypes, false);
}

const char* to_string(ID ty) {
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
    }
    return "";
}

}  // namespace Type
