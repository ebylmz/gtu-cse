%option noyywrap

%{
#include "token_classes.h"
int is_op_cc = 0;
%}

%%
    /* KEYWORDS */
and     { printf("%s: KW_AND\n", yytext); } 
or      { printf("%s: KW_OR\n", yytext); }
not     { printf("%s: KW_NOT\n", yytext); }
equal   { printf("%s: KW_EQUAL\n", yytext); }
less    { printf("%s: KW_LESS\n", yytext); }
nil     { printf("%s: KW_NIL\n", yytext); }
list    { printf("%s: KW_LIST\n", yytext); }
append  { printf("%s: KW_APPEND\n", yytext); }
concat  { printf("%s: KW_CONCAT\n", yytext); }
set     { printf("%s: KW_SET\n", yytext); } 
deffun  { printf("%s: KW_DEFFUN\n", yytext); }
for     { printf("%s: KW_FOR\n", yytext); } 
if      { printf("%s: KW_IF\n", yytext); }
exit    { printf("%s: KW_EXIT\nByee.\n", yytext); return 0;}
load    { printf("%s: KW_LOAD\n", yytext); }
disp    { printf("%s: KW_DISP\n", yytext); }
true    { printf("%s: KW_TRUE\n", yytext); }
false   { printf("%s: KW_FALSE\n", yytext); }

    /* OPERATORS */
[+]     { printf("%s: OP_PLUS\n", yytext); }
[-]     { printf("%s: OP_MINUS\n", yytext); }
[/]     { printf("%s: OP_DIV\n", yytext); }
[*]     { printf("%s: OP_MULT\n", yytext); }
[(]     { printf("%s: OP_OP\n", yytext); }
[)]     { printf("%s: OP_CP\n", yytext); }
[*]{2}  { printf("%s: OP_DBLMULT\n", yytext); }
["] { 
    if (is_op_cc) {
        printf("%s: OP_CC\n", yytext);
        is_op_cc = 0;
    }
    else {
        printf("%s: OP_OC\n", yytext);
        is_op_cc = 1; 
    }
}
[,] { printf("%s: OP_COMMA\n", yytext); }


(;;).*                      { printf("%s: COMMENT\n", yytext); }
[\"].*[\"]                  { printf("%s: VALUESTR\n", yytext); }
[a-zA-Z_][a-zA-Z0-9_]*      { printf("%s: IDENTIFIER\n", yytext); }
[1-9][0-9]*f[1-9][0-9]*     { printf("%s: VALUEF\n", yytext); }
[1-9][0-9]*|0               { printf("%s: VALUEI\n", yytext); }
[ \t\n]                     /* ignore spaces, tabs and newlines */

    /* LEXICAL ERRORS */
[0]+[a-zA-Z][_a-zA-Z0-9]* { 
    printf("%s: LEXICAL ERROR (LEADING DIGIT)\n", yytext); } 
[0-9][0-9]* { 
    printf("%s: LEXICAL ERROR (LEADING ZERO)\n", yytext); }
[a-zA-z0-9+\-/*_,]*[^a-zA-z0-9+\-/*()_,\t\n ]+[a-zA-z0-9+\-/*_,]* { 
    printf("%s: LEXICAL ERROR (APPEARANCE OF ILLEGAL CHRARACTERS)\n", yytext); } 
. { 
    printf("%s: LEXICAL ERROR (SPELLING ERROR)\n", yytext); } 
%%

int main (int argc, char ** argv) {
    FILE * fp = NULL;
    char filename[50];

    if (argc == 1) {
        printf("To terminate the execution, enter exit keyword\n\n");
    }
    else if (argc == 2) {
        fp = fopen(argv[1], "r");
        if (fp) yyin = fp;
    }
    else {
        printf("More than one command line arguments\n");
    }

    yylex();

    if (fp != NULL) fclose(fp);
}
