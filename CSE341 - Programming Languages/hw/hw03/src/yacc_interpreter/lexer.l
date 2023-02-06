%{
	#include "gpp.h"
	#include "parser.tab.h"

	int line_no = 1;
%}

%%
"nil"		{ return KW_NIL; }
"defvar"	{ return KW_DEFV; }
"deffun"	{ return KW_DEFF; }
"if"		{ return KW_IF; }
"exit"		{ return KW_EXIT; }
"load"		{ return KW_LOAD; }
"disp"		{ return KW_DISP; }
"true"		{ return KW_TRUE; }
"false"		{ return KW_FALSE; }
"+"			{ return OP_PLUS; }
"-"			{ return OP_MINUS; }
"/"			{ return OP_DIV; }
"*"			{ return OP_MULT; }
"="			{ return OP_EQ; }
">"			{ return OP_GT; }
"("			{ return OP_OP; }
")"			{ return OP_CP; }
","			{ return OP_COMMA; }
"set"		{ return OP_SET; }
"and"		{ return OP_AND; }
"or" 		{ return OP_OR; }
"not"		{ return OP_NOT; }
";;".*		{ ; /* Ignore */ } 	

[a-zA-Z_][a-zA-Z0-9_]*     	 		{ strcpy(yylval.str, yytext); return ID; }
[-]?([1-9][0-9]*|0)f([1-9][0-9]*|0)     { yylval.valuef = valuef_convert(yytext); return VALUEF; }

^\n							{ if (yyin == stdin) return 0; }
[ \t\r	]                   { ; }
\n						  	{ ++line_no; }
<<EOF>>						{ return 0; }

[0]+[a-zA-Z][_a-zA-Z0-9]* 	{ printf("%s: LEXICAL ERROR (LEADING DIGIT)\n", yytext); return 0; } 
[0-9][0-9]* 				{ printf("%s: LEXICAL ERROR (LEADING ZERO)\n", yytext); return 0; }
[a-zA-z0-9+\-/*_,]*[^a-zA-z0-9+\-/*()_,\t\n ]+[a-zA-z0-9+\-/*_,]* 	{ printf("%s: LEXICAL ERROR (APPEARANCE OF ILLEGAL CHRARACTERS)\n", yytext); } 
. 							{ printf("%s(%d): LEXICAL ERROR (SPELLING ERROR)\n", yytext, *yytext); return 0; } 
%%

int yywrap() {
    return 1;
}