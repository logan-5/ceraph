#include "ast.hpp"
#include "operator.hpp"
#include "parser.hpp"

#include <iostream>

extern "C" int yywrap() {
    return 1;
}

void yy::parser::error(const std::string& err) {
    std::cerr << err;
}

int main(int argc, char** argv) {
    // std::ios::sync_with_stdio(false);
    yy::parser parser;
    parser.parse();
    return 0;
}