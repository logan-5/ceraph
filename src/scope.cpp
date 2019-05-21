#include "scope.hpp"

#include "llvm/IR/Value.h"

namespace scope {
SymbolTable::SymbolTable() : tables(1) {}

bool SymbolTable::exists(llvm::StringRef name) const {
    assert(!tables.empty());
    return tables.back().find(name) != tables.back().end();
}
llvm::AllocaInst* SymbolTable::get(llvm::StringRef name) const {
    if (auto it = tables.back().find(name); it != tables.back().end())
        return it->getValue();
    return nullptr;
}

void SymbolTable::insert(llvm::StringRef name, llvm::AllocaInst* value) {
    assert(!exists(name));
    tables.back()[name] = value;
}

void SymbolTable::pushScope() {
    tables.emplace_back();
}
void SymbolTable::popScope() {
    tables.pop_back();
    assert(!tables.empty());
}

}  // namespace scope