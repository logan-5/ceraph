#ifndef CERAPH_FWD_HPP
#define CERAPH_FWD_HPP

#include <cassert>

#define YY_DECL                                        \
    int yylex(yy::parser::semantic_type* const yylval, \
              const LexerContext& context)

namespace codegen {
struct CodeGenInstance;
}
namespace sema {
struct GetType;
}

#endif
