#include "parser.hpp"

#include <iostream>

extern "C" int yywrap() { return 1; }
int yyerror(const char* err) {
    std::cerr << err;
    return 1;
}

int main(int argc, char** argv) {
    // std::ios::sync_with_stdio(false);
    yyparse();
    return 0;
}