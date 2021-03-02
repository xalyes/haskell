
%%

[a-z]  { yylval = strdup(yytext); return LETTER;}
.|\n { return yytext[0]; }

