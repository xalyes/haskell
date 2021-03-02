%%
[ ]     { /* skip blanks */ }
"true"  { yylval = 1; return BOOL; }
"false" { yylval = 0; return BOOL; }
"and"   { return AND; }
"or"    { return OR; }
"not"   { return NOT; }
\n|.    { return yytext[0]; }
