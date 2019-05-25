#ifndef CERAPH_SCOPE_HPP
#define CERAPH_SCOPE_HPP

#include "llvm/ADT/StringMap.h"

#include <vector>

namespace scope {

template <typename T, typename EmptyT = std::nullptr_t>
class SymbolTable {
    static_assert(std::is_constructible_v<T, EmptyT>);

   public:
    SymbolTable() : tables(1) {}

    bool exists(llvm::StringRef name) const {
        assert(!tables.empty());
        for (auto tableIt = tables.rbegin(); tableIt != tables.rend();
             ++tableIt) {
            if (tableIt->find(name) != tableIt->end())
                return true;
        }
        return false;
    }
    T get(llvm::StringRef name) const {
        for (auto tableIt = tables.rbegin(); tableIt != tables.rend();
             ++tableIt) {
            if (auto it = tableIt->find(name); it != tableIt->end())
                return it->getValue();
        }
        return EmptyT{};
    }

    void insert(llvm::StringRef name, T value) {
        assert(!exists(name));
        tables.back()[name] = std::move(value);
    }
    void insertOrOverwrite(llvm::StringRef name, T value) {
        tables.back()[name] = std::move(value);
    }

    void pushScope() { tables.emplace_back(); }
    void popScope() {
        tables.pop_back();
        assert(!tables.empty());
    }

   private:
    std::vector<llvm::StringMap<T>> tables;
};

}  // namespace scope

#endif
