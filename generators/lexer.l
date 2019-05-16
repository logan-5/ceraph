%{

#include <iostream>

%}

%%
"hello" { std::cout << "i have recognized a token"; }
%%

// code