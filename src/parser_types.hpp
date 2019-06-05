#ifndef PARSER_TYPES_HPP
#define PARSER_TYPES_HPP

#include "type.hpp"

#include <functional>
#include <optional>
#include <string>

// for special intermediate types needed by the parser

// (as far as I can tell) there doesn't seem to be a way to inject declarations
// into the top of bison-generated parser.hpp.
// so this file is the workaround

namespace codegen {
struct CodeGenInstance;
}

struct LexerContext {
    LexerContext(const codegen::CodeGenInstance& i) : instance{i} {}
    std::reference_wrapper<const codegen::CodeGenInstance> instance;
    std::optional<Type::ID> getType(llvm::StringRef name) const;
};

#endif
