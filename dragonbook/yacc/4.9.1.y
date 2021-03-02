%{
#include <stdio.h>
%}

%token BOOL
%token OR
%token AND 
%token NOT

%right AND 
%right OR

%%

lines   : lines bexpr '\n' { printf("%d\n", $2); }
	| lines '\n'
        | /* empty */
	;

bexpr   : bexpr OR bterm { $$ = $1 || $3; }
        | bterm
        ;

bterm   : bterm AND bfactor { $$ = $1 && $3; }
        | bfactor
        ;

bfactor : NOT bfactor { $$ = !$2; }
	| '(' bexpr ')' { $$ = $2; }
        | BOOL 
        ;

%%
#include "lex.yy.c" 
