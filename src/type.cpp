#include "type.hpp"

#include "llvm/IR/DerivedTypes.h"

namespace Type {

llvm::Type* get_type(ID theType, llvm::LLVMContext& theContext) {
    switch (theType) {
        case ID::Int:
            return llvm::IntegerType::get(theContext, num_bits<ID::Int>::value);
        case ID::Float:
            return llvm::Type::getFloatTy(theContext);
        case ID::Double:
            return llvm::Type::getDoubleTy(theContext);
        case ID::StringLiteral:
            assert(false && "I dunno how to implement this yet. I'm sorry");
            break;
    }
    assert(false);
    return nullptr;
}

}  // namespace Type
