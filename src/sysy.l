%option noyywrap
%option nounput
%option noinput

%{
#include <cstdlib>
#include <string>
#include "sysy.tab.hpp"

%}

/* Definitions */

/* Whitespace and comments */
Whitespace   [ \t\r\n]*
LineComment  "//".*
BlockComment "/*"([^*]|\*+[^*/])*\*+"/"

/* Identifiers */
Identifier  [a-zA-Z_][a-zA-Z0-9_]*

/* Integers */
Decimal     [1-9][0-9]*
Octal       0[0-7]*
Hexadecimal 0[xX][0-9a-fA-F]+

%%

{Whitespace}    { /* Ignore whitespace */ }
{LineComment}  { /* Ignore line comments */ }
{BlockComment} { /* Ignore block comments */ }
"int"           {return INT;}
"return"        {return RETURN;}
{Identifier}    { yylval.str_val  = new std::string(yytext); return IDENT; }
{Decimal}       { yylval.int_val = strtol(yytext, nullptr, 0); return INT_CONST; }
{Octal}         { yylval.int_val = strtol(yytext, nullptr, 0); return INT_CONST; }
{Hexadecimal}   { yylval.int_val = strtol(yytext, nullptr, 0); return INT_CONST; }
.               {return yytext[0]; }

%%