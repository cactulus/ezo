%option noyywrap
%option yylineno
%{
#include "ezo.h"
#include "parser.tab.h"

%}

%%

continue { return CONTINUE; }
break { return BREAK; }
new { return NEW; }
nil { return NIL; }
sizeof { return SIZEOF; }
use { return USE; }
struct { return STRUCT; }
enum { return ENUM; }
let { return LET; }
const { return CONST; }
if { return IF; }
else { return ELSE; }
for { return FOR; }
in { return IN; }
while { return WHILE; }
extern { return EXTERN; }
return { return RETURN; }

[a-zA-Z_][a-zA-Z0-9_]* { strcpy(yylval.text, yytext); return ID; }
\"([^\\\"]|\\.)*\" { char *string_lit = escape_str_lit(yytext);
                     strcpy(yylval.text, string_lit);
                     return STRING_LIT; }
[0-9]+ { yylval.int_val = atoi(yytext); return NUMBER; }

"::" { return COLONCOLON; }
"." { return '.'; }
".." { return DDOT; }
"..." { return TDOT; }
":" { return ':'; }
"," { return ','; }
"(" { return OP; }
")" { return CP; }
"{" { return '{'; }
"}" { return '}'; }
"[" { return '['; }
"]" { return ']'; }
"=" { return '='; }
"+" { return ADD; }
"-" { return SUB; }
"*" { return STAR; }
"/" { return DIV; }
"%" { return MOD; }
"&" { return AND; }
"|" { return BOR; }
"!" { return EXCL; }
"++" { return PP; }
"--" { return MM; }
"^" { return BXOR; }
"<" { return LT; }
">" { return GT; }
"<=" { return LE; }
">=" { return GE; }
"==" { return EQEQ; }
"!=" { return NOTEQ; }
"<<" { return BLSH; }
">>" { return BRSH; }
"+=" { return PLUS_EQ; }
"-=" { return SUB_EQ; }
"*=" { return MUL_EQ; }
"/=" { return DIV_EQ; }
"%=" { return MOD_EQ; }
";" { return ';'; }
"&=" { return AND_EQ; }
"|=" { return BOR_EQ; }
"^=" { return BXOR_EQ; }
"<<=" { return BLSH_EQ; }
">>=" { return BRSH_EQ; }
"@" { return AT; }
"&&" { return ANDAND; }
"||" { return BARBAR; }
 
\/\/[^\r\n]* |
\/\*(.*\n)*.*\*\/ |
[ \t]+ {}

[\n\r]  { }

<<EOF>> { yyterminate(); return 0; }
%%
