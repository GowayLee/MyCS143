#ifndef SEMANT_H_
#define SEMANT_H_

#include <assert.h>
#include <iostream>
#include <set>
#include <map>
#include <vector>
#include <string>
#include "cool-tree.h"
#include "stringtab.h"
#include "symtab.h"
#include "list.h"

#define TRUE 1
#define FALSE 0

class ClassTable;
typedef ClassTable *ClassTableP;

class InheritanceGraphNode
{
private:
  Class_ class_;                // The class itself
  InheritanceGraphNode *parent; // The parent node

public:
  InheritanceGraphNode(Class_ class_) : class_(class_), parent((InheritanceGraphNode *)NULL) {}
  Class_ getClass() { return class_; }
  Symbol getName() { return class_->getName(); }
  void setParent(InheritanceGraphNode *node) { parent = node; }
  Symbol getParentName() { return class_->getParentName(); }
  InheritanceGraphNode *getParent() { return parent; }
};

class InheritanceGraph
{
private:
  InheritanceGraphNode *root; // The root of the inheritance graph, always be "Object"
  std::set<InheritanceGraphNode *> *free_node_set;

public:
  InheritanceGraph() : root((InheritanceGraphNode *)NULL), free_node_set(new std::set<InheritanceGraphNode *>()){};
  void setRoot(InheritanceGraphNode *node) { root = node; }
  std::set<InheritanceGraphNode *> *getFreeNodeSet() { return free_node_set; }
  // Load classes in program into free_node_set
  InheritanceGraphNode *add_free_node(Class_ c);
  // Build inheritance tree and leave classes with wrong inheritance into free_node_set
  int construct_tree();
};

// Environment mappings
class Env
{
public:
  static SymbolTable<Symbol, Entry> *object_env;                                 // <object-identifier, Type>
  static SymbolTable<Symbol, std::vector<Symbol>> *method_env;                   // <object-identifier, Type>
  static std::map<std::pair<Symbol, Symbol>, Symbol> *attr_map;                  // <<class-name, method-name>, Type-list>
  static std::map<std::pair<Symbol, Symbol>, std::vector<Symbol> *> *method_map; // <<class-name, method-name>, Type-list>
  static Class_ cur_class;
  static std::map<Symbol, InheritanceGraphNode *> *class_map; // Maintain Symbol->InheritanceNode map to speed up the search
};

SymbolTable<Symbol, Entry> *Env::object_env = NULL;
SymbolTable<Symbol, std::vector<Symbol>> *Env::method_env = NULL;
std::map<std::pair<Symbol, Symbol>, Symbol> *Env::attr_map = new std::map<std::pair<Symbol, Symbol>, Symbol>();
std::map<std::pair<Symbol, Symbol>, std::vector<Symbol> *> *Env::method_map = new std::map<std::pair<Symbol, Symbol>, std::vector<Symbol> *>();
Class_ Env::cur_class = (Class_)NULL;
std::map<Symbol, InheritanceGraphNode *> *Env::class_map = new std::map<Symbol, InheritanceGraphNode *>();

// This is a structure that may be used to contain the semantic
// information such as the inheritance graph.  You may use it or not as
// you like: it is only here to provide a container for the supplied
// methods.

class ClassTable
{
private:
  InheritanceGraph *inheritance_graph;

  static int semant_errors;
  static ostream &error_stream;

  void install_basic_classes();

public:
  ClassTable(Classes);
  static int errors() { return semant_errors; }
  static ostream &semant_error();
  static ostream &semant_error(Class_ c);
  static ostream &semant_error(Symbol filename, tree_node *t);

  // Inheritance graph check
  void check_inheritance_graph(Classes classes);
  // Anaylsis errors in the nonempty free_node_set
  void analysis_inheritance_error();
  static int install_features(Classes classes);
  static int setup_environment(Class_ cur_class);
  static List<Class__class> *get_ancestors(Class_ cur_class);
  static bool check_subtype(Symbol type1, Symbol type2);
  static Symbol get_LCA(Symbol type1, Symbol type2);
  static Symbol get_LCA(std::vector<Symbol> *type_list);
};

int ClassTable::semant_errors = 0;
ostream &ClassTable::error_stream = cerr;

#endif