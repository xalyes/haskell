%{
#include <stdio.h>
#include <string.h>
#define YYSTYPE char*
%}

%token LETTER

%%

line   : S '\n' { printf("%s\n", $1); }
       ;

S   : '(' L ')' { $$ = $2;}
    | LETTER
    ;

L   : L ',' S  {
                   char* s = malloc(sizeof(char)*(strlen($1)+strlen($3)+1));
                   strcpy(s,$1); strcat(s,$3);
                   $$=s;
                   free($1);
                   free($3);
               }
    | S       
    ;

%%
#include "lex.yy.c" 
