//
// The following include files must come first.

#ifndef COOL_TREE_HANDCODE_H
#define COOL_TREE_HANDCODE_H

#include <iostream>
#include "tree.h"
#include "cool.h"
#include "stringtab.h"
#define yylineno curr_lineno;
extern int yylineno;

inline Boolean copy_Boolean(Boolean b) { return b; }
inline void assert_Boolean(Boolean) {}
inline void dump_Boolean(ostream &stream, int padding, Boolean b)
{
  stream << pad(padding) << (int)b << "          \n";
}

void dump_Symbol(ostream &stream, int padding, Symbol b);
void assert_Symbol(Symbol b);
Symbol copy_Symbol(Symbol b);

class Program_class;
typedef Program_class *Program;
class Class__class;
typedef Class__class *Class_;
class Feature_class;
typedef Feature_class *Feature;
class Formal_class;
typedef Formal_class *Formal;
class Expression_class;
typedef Expression_class *Expression;
class Case_class;
typedef Case_class *Case;

typedef list_node<Class_> Classes_class;
typedef Classes_class *Classes;
typedef list_node<Feature> Features_class;
typedef Features_class *Features;
typedef list_node<Formal> Formals_class;
typedef Formals_class *Formals;
typedef list_node<Expression> Expressions_class;
typedef Expressions_class *Expressions;
typedef list_node<Case> Cases_class;
typedef Cases_class *Cases;

#define Program_EXTRAS                              \
  virtual void semant() = 0;                        \
  virtual void dump_with_types(ostream &, int) = 0; \
  virtual void type_check() = 0;

#define program_EXTRAS                  \
  void semant();                        \
  void dump_with_types(ostream &, int); \
  void type_check();

#define Class__EXTRAS                               \
  virtual Symbol get_filename() = 0;                \
  virtual Symbol getName() = 0;                     \
  virtual Symbol getParentName() = 0;               \
  virtual Features getFeatures() = 0;               \
  virtual Symbol getFileName() = 0;                 \
  virtual void dump_with_types(ostream &, int) = 0; \
  virtual void type_check() = 0;

#define class__EXTRAS                         \
  Symbol get_filename() { return filename; }  \
  Symbol getName() { return name; }           \
  Symbol getParentName() { return parent; }   \
  Features getFeatures() { return features; } \
  Symbol getFileName() { return filename; }   \
  void dump_with_types(ostream &, int);       \
  void type_check();

#define Feature_EXTRAS                              \
  virtual void dump_with_types(ostream &, int) = 0; \
  virtual bool isMethod() = 0;                      \
  virtual void type_check() = 0;

#define Feature_SHARED_EXTRAS \
  void dump_with_types(ostream &, int);

#define method_EXTRAS                            \
  bool isMethod() { return true; }               \
  Symbol getName() { return name; }              \
  Formals getFormals() { return formals; }       \
  Symbol getReturnType() { return return_type; } \
  Expression getExpr() { return expr; }          \
  void type_check();

#define attr_EXTRAS                         \
  bool isMethod() { return false; }         \
  Symbol getName() { return name; }         \
  Symbol getType() { return type_decl; }    \
  Expression getInitExpr() { return init; } \
  void type_check();

#define Formal_EXTRAS                               \
  virtual void dump_with_types(ostream &, int) = 0; \
  virtual Symbol getName() = 0;                     \
  virtual Symbol getType() = 0;

#define formal_EXTRAS                   \
  void dump_with_types(ostream &, int); \
  Symbol getName() { return name; }     \
  Symbol getType() { return type_decl; }

#define Case_EXTRAS \
  virtual void dump_with_types(ostream &, int) = 0;

#define branch_EXTRAS \
  void dump_with_types(ostream &, int); \
  Symbol getName() { return name; } \
  Symbol getTypeDecl() { return type_decl; } \
  Expression getExpr() { return expr; }

#define Expression_EXTRAS                           \
  Symbol type;                                      \
  Symbol get_type() { return type; }                \
  Expression set_type(Symbol s)                     \
  {                                                 \
    type = s;                                       \
    return this;                                    \
  }                                                 \
  virtual void dump_with_types(ostream &, int) = 0; \
  void dump_type(ostream &, int);                   \
  Expression_class() { type = (Symbol)NULL; }       \
  virtual void type_check() = 0;

#define Expression_SHARED_EXTRAS \
  void dump_with_types(ostream &, int);

#define assign_EXTRAS \
  void type_check();

#define static_dispatch_EXTRAS \
  void type_check();

#define dispatch_EXTRAS \
  void type_check();

#define cond_EXTRAS \
  void type_check();

#define loop_EXTRAS \
  void type_check();

#define typcase_EXTRAS \
  void type_check();

#define block_EXTRAS \
  void type_check();

#define let_EXTRAS \
  void type_check();

#define plus_EXTRAS \
  void type_check();

#define sub_EXTRAS \
  void type_check();

#define mul_EXTRAS \
  void type_check();

#define divide_EXTRAS \
  void type_check();

#define neg_EXTRAS \
  void type_check();

#define lt_EXTRAS \
  void type_check();

#define eq_EXTRAS \
  void type_check();

#define leq_EXTRAS \
  void type_check();

#define comp_EXTRAS \
  void type_check();

#define int_const_EXTRAS \
  void type_check();

#define bool_const_EXTRAS \
  void type_check();

#define string_const_EXTRAS \
  void type_check();

#define new__EXTRAS \
  void type_check();

#define isvoid_EXTRAS \
  void type_check();

#define no_expr_EXTRAS \
  void type_check();

#define object_EXTRAS \
  void type_check();

#endif
