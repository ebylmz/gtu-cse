/* A Bison parser, made by GNU Bison 3.8.2.  */

/* Bison interface for Yacc-like parsers in C

   Copyright (C) 1984, 1989-1990, 2000-2015, 2018-2021 Free Software Foundation,
   Inc.

   This program is free software: you can redistribute it and/or modify
   it under the terms of the GNU General Public License as published by
   the Free Software Foundation, either version 3 of the License, or
   (at your option) any later version.

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with this program.  If not, see <https://www.gnu.org/licenses/>.  */

/* As a special exception, you may create a larger work that contains
   part or all of the Bison parser skeleton and distribute that work
   under terms of your choice, so long as that work isn't itself a
   parser generator using the skeleton or a modified version thereof
   as a parser skeleton.  Alternatively, if you modify or redistribute
   the parser skeleton itself, you may (at your option) remove this
   special exception, which will cause the skeleton and the resulting
   Bison output files to be licensed under the GNU General Public
   License without this special exception.

   This special exception was added by the Free Software Foundation in
   version 2.2 of Bison.  */

/* DO NOT RELY ON FEATURES THAT ARE NOT DOCUMENTED in the manual,
   especially those whose name start with YY_ or yy_.  They are
   private implementation details that can be changed or removed.  */

#ifndef YY_YY_PARSER_TAB_H_INCLUDED
# define YY_YY_PARSER_TAB_H_INCLUDED
/* Debug traces.  */
#ifndef YYDEBUG
# define YYDEBUG 0
#endif
#if YYDEBUG
extern int yydebug;
#endif

/* Token kinds.  */
#ifndef YYTOKENTYPE
# define YYTOKENTYPE
  enum yytokentype
  {
    YYEMPTY = -2,
    YYEOF = 0,                     /* "end of file"  */
    YYerror = 256,                 /* error  */
    YYUNDEF = 257,                 /* "invalid token"  */
    KW_NIL = 258,                  /* KW_NIL  */
    KW_DEFV = 259,                 /* KW_DEFV  */
    KW_DEFF = 260,                 /* KW_DEFF  */
    KW_WHILE = 261,                /* KW_WHILE  */
    KW_IF = 262,                   /* KW_IF  */
    KW_EXIT = 263,                 /* KW_EXIT  */
    KW_LOAD = 264,                 /* KW_LOAD  */
    KW_DISP = 265,                 /* KW_DISP  */
    KW_TRUE = 266,                 /* KW_TRUE  */
    KW_FALSE = 267,                /* KW_FALSE  */
    OP_PLUS = 268,                 /* OP_PLUS  */
    OP_MINUS = 269,                /* OP_MINUS  */
    OP_DIV = 270,                  /* OP_DIV  */
    OP_MULT = 271,                 /* OP_MULT  */
    OP_OP = 272,                   /* OP_OP  */
    OP_CP = 273,                   /* OP_CP  */
    OP_SET = 274,                  /* OP_SET  */
    OP_COMMA = 275,                /* OP_COMMA  */
    OP_AND = 276,                  /* OP_AND  */
    OP_OR = 277,                   /* OP_OR  */
    OP_NOT = 278,                  /* OP_NOT  */
    OP_EQ = 279,                   /* OP_EQ  */
    OP_GT = 280,                   /* OP_GT  */
    COMMENT = 281,                 /* COMMENT  */
    VALUEF = 282,                  /* VALUEF  */
    ID = 283                       /* ID  */
  };
  typedef enum yytokentype yytoken_kind_t;
#endif
/* Token kinds.  */
#define YYEMPTY -2
#define YYEOF 0
#define YYerror 256
#define YYUNDEF 257
#define KW_NIL 258
#define KW_DEFV 259
#define KW_DEFF 260
#define KW_WHILE 261
#define KW_IF 262
#define KW_EXIT 263
#define KW_LOAD 264
#define KW_DISP 265
#define KW_TRUE 266
#define KW_FALSE 267
#define OP_PLUS 268
#define OP_MINUS 269
#define OP_DIV 270
#define OP_MULT 271
#define OP_OP 272
#define OP_CP 273
#define OP_SET 274
#define OP_COMMA 275
#define OP_AND 276
#define OP_OR 277
#define OP_NOT 278
#define OP_EQ 279
#define OP_GT 280
#define COMMENT 281
#define VALUEF 282
#define ID 283

/* Value type.  */
#if ! defined YYSTYPE && ! defined YYSTYPE_IS_DECLARED
union YYSTYPE
{
#line 27 "parser.y"

    Valuef valuef;
    int valueb;
    char str[16];

#line 129 "parser.tab.h"

};
typedef union YYSTYPE YYSTYPE;
# define YYSTYPE_IS_TRIVIAL 1
# define YYSTYPE_IS_DECLARED 1
#endif


extern YYSTYPE yylval;


int yyparse (void);


#endif /* !YY_YY_PARSER_TAB_H_INCLUDED  */
