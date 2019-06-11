#include "ceraph_fwd.hpp"

#include "ast.hpp"
#include "codegen.hpp"
#include "operator.hpp"
#include "parser_types.hpp"
#include "sema.hpp"
#include "util.hpp"

#include "parser.hpp"

#include <cstdio>
#include <iostream>

extern "C" int yywrap() {
    return 1;
}

void yy::parser::error(const std::string& err) {
    std::cerr << err << '\n';
}

// pointlessly "optimized" code alert
template <typename ErrWrapper>
using success_value = typename ErrWrapper::storage_type;

auto open_file(const char* path) {
    extern FILE* yyin;
    yyin = std::fopen(path, "r");
    auto cleanup = [f = yyin] { std::fclose(f); };
    using CleanupType = std::unique_ptr<util::ScopeGuard<decltype(cleanup)>>;
    using ReturnType = llvm::ErrorOr<CleanupType>;
    if (!yyin) {
        std::cerr << "can't open file " << path << ": [Errno " << errno
                  << "]: " << strerror(errno) << '\n';
        return ReturnType{std::make_error_code(static_cast<std::errc>(errno))};
    }
    return ReturnType{std::make_unique<CleanupType::element_type>(cleanup)};
}

int main(int argc, char** argv) {
    // std::ios::sync_with_stdio(false);
    const auto fileHandleOrErr =
          argc > 1 ? open_file(argv[1])
                   : success_value<decltype(open_file("hi"))>{};
    if (!fileHandleOrErr) {
        return fileHandleOrErr.getError().value();
    }

    codegen::CodeGenInstance instance;
    sema::GetType typechecker{instance.getTypeTable()};
    yy::parser parser{instance, typechecker};
    parser.parse();
    instance.dump(llvm::outs());
    return 0;
}
