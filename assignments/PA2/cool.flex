/*
 *  The scanner definition for COOL.
 */

/*
 *  Stuff enclosed in %{ %} in the first section is copied verbatim to the
 *  output, so headers and global definitions are placed here to be visible
 * to the code in the file.  Don't remove anything that was here initially
 */
%{
#include <cool-parse.h>
#include <stringtab.h>
#include <utilities.h>

/* The compiler assumes these identifiers. */
#define yylval cool_yylval
#define yylex  cool_yylex

/* Max size of string constants */
#define MAX_STR_CONST 1025
#define YY_NO_UNPUT   /* keep g++ happy */

extern FILE *fin; /* we read from this file */

/* define YY_INPUT so we read from the FILE fin:
 * This change makes it possible to use this scanner in
 * the Cool compiler.
 */
#undef YY_INPUT
#define YY_INPUT(buf,result,max_size) \
	if ( (result = fread( (char*)buf, sizeof(char), max_size, fin)) < 0) \
		YY_FATAL_ERROR("read() in flex scanner failed");

char string_buf[MAX_STR_CONST]; /* to assemble string constants */
char *string_buf_ptr;

extern int curr_lineno;
extern int verbose_flag;

extern YYSTYPE cool_yylval;

/*	*****************************
 *  Add Your own definitions here
 *  *****************************
 */
#include <stdlib.h> /* atoi */

// String relavted
#define MAX_STR_BUF_LOC string_buf + MAX_STR_CONST - 1
bool is_escape = false;
void reset_string_buf() {
	/* Reset string buffer once detect \"
	 *  and ready to record string as stream.
	 */
	string_buf[0] = '\0';
	string_buf_ptr = string_buf;
	is_escape = false;
}
char get_escaped_char(const char c) {
  /* Get escaped char */
	switch (c) {
		case 'n': return '\n';
		case 't': return '\t';
		case 'b': return '\b';
		case 'f': return '\f';
		case '\\': return '\\';
		case '\"': return '\"';
    case '\'': return '\'';
		default: return c;
	}
}

// Comment related
int nested_comment_level = 0;

// // Record column number
// extern int curr_columnno;
// #define YY_USER_ACTION update_position();

// int prev_lineno = 1;
// int prev_yyleng = yyleng;
// void update_position() {
  // if (yytext[0] == '\0') return;
  // if (prev_lineno != curr_lineno) {
    // curr_columnno = 0;
    // prev_lineno = curr_lineno;
  // }
  // curr_columnno += prev_yyleng;
  // prev_yyleng = yyleng;
// }

%}

/*
 * User states.
 */
%x MULTILINE_COMMENT INLINE_COMMENT NORMAL_STRING_CONST ERROR_STRING_CONST

/*
 * Define names for regular expressions here.
 */

DELIME_EXNL  	                                      [ \t\f\v\r]
                              
NUMBER		                                          -?[0-9]+
OBJECTID		                                        [a-z][a-zA-Z0-9_]*
TYPEID		                                          [A-Z][a-zA-Z0-9_]*
                              
SINGLE_CHAR_OPTER		                                [-+*\/:~<=(){};.,@]
                              
DARROW		                                          =>
ASSIGN                                              <-
LE                                                  <=


%%

 /*
  *  Nested nested_comments
  */
"(*"                                                {
  nested_comment_level++;
  BEGIN(MULTILINE_COMMENT);
}
<MULTILINE_COMMENT>"(*"                             nested_comment_level++;
<MULTILINE_COMMENT>\n                               curr_lineno++;
<MULTILINE_COMMENT>"*)"                             if (--nested_comment_level == 0) BEGIN(INITIAL);
<MULTILINE_COMMENT><<EOF>>                          {
  BEGIN(INITIAL);
  cool_yylval.error_msg = "EOF in nested comment";
  return ERROR;
}
<MULTILINE_COMMENT>.                                ;
"*)"                                                {
  if (nested_comment_level == 0) {
    cool_yylval.error_msg = "Unmatched *)";
    return ERROR;
  }
}


 /*
  *  Inline comments
  */
"--"                                                BEGIN(INLINE_COMMENT);
<INLINE_COMMENT>\n                                  {
  curr_lineno++;
  BEGIN(INITIAL);
}
<INLINE_COMMENT>.                                   ;


 /*
  *  White spaces
  */
\n                                                  curr_lineno++;
{DELIME_EXNL}+                                      ;


 /*
  *  The multiple-character operators.
  */
{DARROW}		                                        return DARROW;
{ASSIGN}		                                        return ASSIGN;
{LE}		                                            return LE;


 /*
  *  Single-character operators.
  */
{SINGLE_CHAR_OPTER}                                 return yytext[0];


 /*
  * Keywords are case-insensitive except for the values true and false,
  * which must begin with a lower-case letter.
  */
(?i:class)		                                      return CLASS;
(?i:else)		                                        return ELSE;
(?i:fi)		                                          return FI;
(?i:if)		                                          return IF;
(?i:in)		                                          return IN;
(?i:inherits)		                                    return INHERITS;
(?i:let)		                                        return LET;
(?i:loop)		                                        return LOOP;
(?i:pool)		                                        return POOL;
(?i:then)		                                        return THEN;
(?i:while)		                                      return WHILE;
(?i:case)		                                        return CASE;
(?i:esac)		                                        return ESAC;
(?i:new)		                                        return NEW;
(?i:of)		                                          return OF;
(?i:isvoid)		                                      return ISVOID;
(?i:not)		                                        return NOT;


 /*
  *  Identifiers.
  */
{TYPEID}                                            {
  cool_yylval.symbol = idtable.add_string(yytext);
  return TYPEID;
}

{OBJECTID}                                          {
  cool_yylval.symbol = idtable.add_string(yytext);
  return OBJECTID;
}


 /*
  *  Integers.
  */
{NUMBER}                                         {
  cool_yylval.symbol = inttable.add_int(atoi(yytext));
  return INT_CONST;
}

 /*
  *  Booleans.
  */
t(?i:rue)                                           cool_yylval.boolean = true;  return BOOL_CONST;
f(?i:alse)                                          cool_yylval.boolean = false; return BOOL_CONST;

 /*
  *  String constants (C syntax)
  *  Escape sequence \c is accepted for all characters c. Except for 
  *  \n \t \b \f, the result is c.
  *
  */
\"																									{
	BEGIN(NORMAL_STRING_CONST);
	reset_string_buf();
}
<NORMAL_STRING_CONST>\\															is_escape = true;
<NORMAL_STRING_CONST>\n															{
  curr_lineno++;
  if (is_escape) {
    is_escape = false;
  } else {
    // Assume the programmer simply forget the close-quote.
    BEGIN(ERROR_STRING_CONST);
    cool_yylval.error_msg = "Unterminated string constant";
    return ERROR;
  }
}
<NORMAL_STRING_CONST>\"															{
  *string_buf_ptr = '\0';
  BEGIN(INITIAL);
	cool_yylval.symbol = stringtable.add_string(string_buf);
	return STR_CONST;
}
<NORMAL_STRING_CONST>[^\0]*\0                       {
  BEGIN(ERROR_STRING_CONST);
  cool_yylval.error_msg = "String contains null character";
  return ERROR;
}
<NORMAL_STRING_CONST><<EOF>>                        {
  BEGIN(INITIAL);
  cool_yylval.error_msg = "EOF in string constant";
  return ERROR;
}
<NORMAL_STRING_CONST>.															{
	if (is_escape) {
		const char escaped_char = get_escaped_char(yytext[0]);
    *string_buf_ptr = escaped_char;
		is_escape = false;
	} else {
		*string_buf_ptr = yytext[0];
  }

  // Check if the string is too long
  if (++string_buf_ptr >= MAX_STR_BUF_LOC) {
    BEGIN(ERROR_STRING_CONST);
    cool_yylval.error_msg = "String constant too long";
    return ERROR;
  }
}

<ERROR_STRING_CONST>\"												      BEGIN(INITIAL);
<ERROR_STRING_CONST>\n                              curr_lineno++; BEGIN(INITIAL);
<ERROR_STRING_CONST>.                               ;

.                                                   {
  cool_yylval.error_msg = yytext;
  return ERROR;
}

%%
