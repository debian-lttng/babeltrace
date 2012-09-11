/* A Bison parser, made by GNU Bison 2.6.2.  */

/* Bison interface for Yacc-like parsers in C
   
      Copyright (C) 1984, 1989-1990, 2000-2012 Free Software Foundation, Inc.
   
   This program is free software: you can redistribute it and/or modify
   it under the terms of the GNU General Public License as published by
   the Free Software Foundation, either version 3 of the License, or
   (at your option) any later version.
   
   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.
   
   You should have received a copy of the GNU General Public License
   along with this program.  If not, see <http://www.gnu.org/licenses/>.  */

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

#ifndef YY_CTF_PARSER_H
# define YY_CTF_PARSER_H
/* Enabling traces.  */
#ifndef YYDEBUG
# define YYDEBUG 1
#endif
#if YYDEBUG
extern int yydebug;
#endif

/* Tokens.  */
#ifndef YYTOKENTYPE
# define YYTOKENTYPE
   /* Put the tokens into the symbol table, so that GDB and other debuggers
      know about them.  */
   enum yytokentype {
     CHARACTER_CONSTANT_START = 258,
     SQUOTE = 259,
     STRING_LITERAL_START = 260,
     DQUOTE = 261,
     ESCSEQ = 262,
     CHAR_STRING_TOKEN = 263,
     LSBRAC = 264,
     RSBRAC = 265,
     LPAREN = 266,
     RPAREN = 267,
     LBRAC = 268,
     RBRAC = 269,
     RARROW = 270,
     STAR = 271,
     PLUS = 272,
     MINUS = 273,
     LT = 274,
     GT = 275,
     TYPEASSIGN = 276,
     COLON = 277,
     SEMICOLON = 278,
     DOTDOTDOT = 279,
     DOT = 280,
     EQUAL = 281,
     COMMA = 282,
     CONST = 283,
     CHAR = 284,
     DOUBLE = 285,
     ENUM = 286,
     ENV = 287,
     EVENT = 288,
     FLOATING_POINT = 289,
     FLOAT = 290,
     INTEGER = 291,
     INT = 292,
     LONG = 293,
     SHORT = 294,
     SIGNED = 295,
     STREAM = 296,
     STRING = 297,
     STRUCT = 298,
     TRACE = 299,
     CLOCK = 300,
     TYPEALIAS = 301,
     TYPEDEF = 302,
     UNSIGNED = 303,
     VARIANT = 304,
     VOID = 305,
     _BOOL = 306,
     _COMPLEX = 307,
     _IMAGINARY = 308,
     DECIMAL_CONSTANT = 309,
     OCTAL_CONSTANT = 310,
     HEXADECIMAL_CONSTANT = 311,
     TOK_ALIGN = 312,
     IDENTIFIER = 313,
     ID_TYPE = 314,
     ERROR = 315
   };
#endif
/* Tokens.  */
#define CHARACTER_CONSTANT_START 258
#define SQUOTE 259
#define STRING_LITERAL_START 260
#define DQUOTE 261
#define ESCSEQ 262
#define CHAR_STRING_TOKEN 263
#define LSBRAC 264
#define RSBRAC 265
#define LPAREN 266
#define RPAREN 267
#define LBRAC 268
#define RBRAC 269
#define RARROW 270
#define STAR 271
#define PLUS 272
#define MINUS 273
#define LT 274
#define GT 275
#define TYPEASSIGN 276
#define COLON 277
#define SEMICOLON 278
#define DOTDOTDOT 279
#define DOT 280
#define EQUAL 281
#define COMMA 282
#define CONST 283
#define CHAR 284
#define DOUBLE 285
#define ENUM 286
#define ENV 287
#define EVENT 288
#define FLOATING_POINT 289
#define FLOAT 290
#define INTEGER 291
#define INT 292
#define LONG 293
#define SHORT 294
#define SIGNED 295
#define STREAM 296
#define STRING 297
#define STRUCT 298
#define TRACE 299
#define CLOCK 300
#define TYPEALIAS 301
#define TYPEDEF 302
#define UNSIGNED 303
#define VARIANT 304
#define VOID 305
#define _BOOL 306
#define _COMPLEX 307
#define _IMAGINARY 308
#define DECIMAL_CONSTANT 309
#define OCTAL_CONSTANT 310
#define HEXADECIMAL_CONSTANT 311
#define TOK_ALIGN 312
#define IDENTIFIER 313
#define ID_TYPE 314
#define ERROR 315



#if ! defined YYSTYPE && ! defined YYSTYPE_IS_DECLARED
typedef union YYSTYPE
{
/* Line 2049 of yacc.c  */
#line 920 "ctf-parser.y"

	long long ll;
	char c;
	struct gc_string *gs;
	struct ctf_node *n;


/* Line 2049 of yacc.c  */
#line 185 "ctf-parser.h"
} YYSTYPE;
# define YYSTYPE_IS_TRIVIAL 1
# define yystype YYSTYPE /* obsolescent; will be withdrawn */
# define YYSTYPE_IS_DECLARED 1
#endif


#ifdef YYPARSE_PARAM
#if defined __STDC__ || defined __cplusplus
int yyparse (void *YYPARSE_PARAM);
#else
int yyparse ();
#endif
#else /* ! YYPARSE_PARAM */
#if defined __STDC__ || defined __cplusplus
int yyparse (struct ctf_scanner *scanner);
#else
int yyparse ();
#endif
#endif /* ! YYPARSE_PARAM */

#endif /* !YY_CTF_PARSER_H  */
