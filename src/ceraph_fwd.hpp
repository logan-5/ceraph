#ifndef CERAPH_FWD_HPP
#define CERAPH_FWD_HPP

#include <range/v3/range_fwd.hpp>

#include <cassert>

namespace rv = ranges::view;

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
