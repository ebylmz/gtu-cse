%{
#include <stdio.h>
#include <stdlib.h>
#include "gpp.h"

#define OUT_FILE "output.txt"

extern FILE * yyin;     /* defined by lex, reads from this file */
extern FILE * yyout;    /* defined by lex, writes to this file */
extern int yyparse();   /* defined by yacc, parser function */
extern char * yytext;   /* defined by lex current token */
extern int line_no;     /* current line */

int yylex();

int yyerror(char* str) { 
    fprintf(yyout, "SYNTAX_ERROR %s Expression at line %d is not recognized\n", yytext, line_no);
    return 0; 
}

SymbolTable * t;

int exit_prog = 0;

%}

%union {
    Valuef valuef;
    int valueb;
    char str[16];
}

%start START

%token KW_NIL KW_DEFV KW_DEFF KW_WHILE KW_IF KW_EXIT KW_LOAD KW_DISP KW_TRUE KW_FALSE

%token OP_PLUS OP_MINUS OP_DIV OP_MULT OP_OP OP_CP OP_SET OP_COMMA OP_AND OP_OR OP_NOT OP_EQ OP_GT

%token COMMENT

%token <valuef> VALUEF

%token <str> ID

%type <valuef> INPUT
%type <valuef> EXP
%type <valueb> EXPB
%type <valuef> EXPLIST
%type <valuef> FUNCTION
%type <valuef> FCALL
%type <valuef> ASG

%%

START   : START INPUT 
        | INPUT 
        ;

INPUT   : FUNCTION      { fprintf(yyout, "Syntax OK.\nResult: %df%d\n", $1.num, $1.denom); }
        | EXP           { fprintf(yyout, "Syntax OK.\nResult: %df%d\n", $1.num, $1.denom); if (exit_prog) return 0; }
        | EXPLIST       { fprintf(yyout, "Syntax OK.\nResult: %df%d\n", $1.num, $1.denom); }
        ;

EXP     : OP_OP OP_PLUS EXP EXP OP_CP                   { $$ = valuef_add($3, $4); }
        | OP_OP OP_MINUS EXP EXP OP_CP                  { $$ = valuef_sub($3, $4); }
        | OP_OP OP_MULT EXP EXP OP_CP                   { $$ = valuef_mult($3, $4); }
        | OP_OP OP_DIV EXP EXP OP_CP                    { $$ = valuef_div($3, $4); }
        | OP_OP KW_IF EXPB EXPLIST EXPLIST OP_CP        { $$ = $3 == 1 ? $4 : $5; } 
        | OP_OP KW_WHILE EXPB EXPLIST OP_CP             { $$ = $3 == 1 ? $4 :  valuef_create(0, 1); }
        | OP_OP KW_DEFV ID EXP OP_CP                    { $$ = $4; sym_tab_def(t, var_t, $3, $4); }
        | OP_OP OP_SET ID EXP OP_CP                     { $$ = $4; sym_tab_set(t, $3, $4); }     
        | OP_OP KW_EXIT OP_CP                           { $$ = valuef_create(1, 0); exit_prog = 1; } /* 1f0 indicates exit program */
        | ID                                            { Entry * e = sym_tab_get(t, var_t, $1); $$ = (e == NULL) ? valuef_create(0, 1) : e->value; }                                      
        | VALUEF                                        { $$ = $1; }
        | FCALL                                         { $$ = $1; }
        | ASG                                           { $$ = $1; }
        ;

EXPB    : OP_OP OP_EQ EXP EXP OP_CP         { $$ = valuef_eq($3, $4); }
        | OP_OP OP_GT EXP EXP OP_CP         { $$ = valuef_gt($3, $4); }
        | KW_TRUE                           { $$ = 1; }
        | KW_FALSE                          { $$ = 0; }
        | OP_OP OP_AND EXPB EXPB OP_CP      { $$ = $3 && $4; }
        | OP_OP OP_OR EXPB EXPB OP_CP       { $$ = $3 || $4; }
        | OP_OP OP_NOT EXPB OP_CP           { $$ = !$3; }
        ;

EXPLIST : OP_OP EXP OP_CP               { $$ = $2; }
        | OP_OP EXPLIST OP_CP           { $$ = $2; }
        ;

ASG     : OP_OP OP_SET ID EXP OP_CP     { $$ = $4; sym_tab_set(t, $3, $4); } 
        ;

FUNCTION: OP_OP KW_DEFF ID OP_OP ID OP_CP EXPLIST OP_CP              { $$ = valuef_create(0, 1); sym_tab_def(t, func_t, $3, valuef_create(0, 1)); }
        | OP_OP KW_DEFF ID OP_OP ID ID OP_CP EXPLIST OP_CP           { $$ = valuef_create(0, 1); sym_tab_def(t, func_t, $3, valuef_create(0, 1)); }
        | OP_OP KW_DEFF ID OP_OP ID ID ID OP_CP EXPLIST OP_CP        { $$ = valuef_create(0, 1); sym_tab_def(t, func_t, $3, valuef_create(0, 1)); }
        ;

FCALL   : OP_OP ID EXP OP_CP                    { Entry * e = sym_tab_get(t, func_t, $2); $$ = (e == NULL) ? valuef_create(0, 1) : e->value; }
        | OP_OP ID EXP EXP OP_CP                { Entry * e = sym_tab_get(t, func_t, $2); $$ = (e == NULL) ? valuef_create(0, 1) : e->value; }
        | OP_OP ID EXP EXP EXP OP_CP            { Entry * e = sym_tab_get(t, func_t, $2); $$ = (e == NULL) ? valuef_create(0, 1) : e->value; }
        ;
%%

int main(int argc, char * argv[]) {
    FILE * in_stream = NULL;
    FILE * out_stream = NULL;

    if (argc == 1) {
        /* shell mode */
        in_stream = stdin;
    }
    else {
        in_stream = fopen(argv[1], "r");
        if (!in_stream) {
            printf("File \"%s\" cannot find or open\n", argv[1]);
            return 1;
        }
    }

    out_stream = fopen(OUT_FILE, "w");

    /* set IO */
    yyin = in_stream;
    yyout = out_stream;

    /* initiliaze the symbol table */
    t = sym_tab_alloc();

    yyparse();
    sym_tab_print(t);

    fclose(out_stream);
    sym_tab_free(t); 
}

