#include "ceraph_fwd.hpp"

#include "ast.hpp"
#include "codegen.hpp"
#include "operator.hpp"
#include "parser_types.hpp"
#include "sema.hpp"

#include "parser.hpp"

#include <iostream>

extern "C" int yywrap() {
    return 1;
}

void yy::parser::error(const std::string& err) {
    std::cerr << err;
}

int main(int, char**) {
    // std::ios::sync_with_stdio(false);
    codegen::CodeGenInstance instance;
    sema::GetType typechecker;
    yy::parser parser{instance, typechecker};
    parser.parse();
    return 0;
}