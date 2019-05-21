#ifndef CERAPH_SCOPE_HPP
#define CERAPH_SCOPE_HPP

#include "llvm/ADT/StringMap.h"

#include <vector>

namespace llvm {
class AllocaInst;
}

namespace scope {

class SymbolTable {
   public:
    SymbolTable();

    bool exists(llvm::StringRef name) const;
    llvm::AllocaInst* get(llvm::StringRef name) const;

    void insert(llvm::StringRef name, llvm::AllocaInst* value);

    void pushScope();
    void popScope();

   private:
    std::vector<llvm::StringMap<llvm::AllocaInst*>> tables;
};

}  // namespace scope

#endif
