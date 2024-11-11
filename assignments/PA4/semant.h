#ifndef SEMANT_H_
#define SEMANT_H_

#include <assert.h>
#include <iostream>
#include <set>
#include <map>
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
  Class_ class_; // The class itself
  InheritanceGraphNode *parent; // The parent node

public:
  InheritanceGraphNode(Class_ class_) : class_(class_) {}
  Class_ getClass() { return class_; }
  Symbol getName() { return class_->getName(); }
  void setParent(InheritanceGraphNode *node) { parent = node; }
  Symbol getParentName() { return class_->getParentName(); }
};

class InheritanceGraph
{
private:
  InheritanceGraphNode *root;    // The root of the inheritance graph, always be "Object"
  std::set<InheritanceGraphNode *> *free_node_set;

public:
  InheritanceGraph() {};
  void setRoot(InheritanceGraphNode *node) { root = node; }
  std::set<InheritanceGraphNode *> *getFreeNodeSet() { return free_node_set; }
  // Load classes in program into free_node_set
  int add_free_node(Class_ c);
  // Build inheritance tree and leave classes with wrong inheritance into free_node_set
  int construct_tree();
  
};

// This is a structure that may be used to contain the semantic
// information such as the inheritance graph.  You may use it or not as
// you like: it is only here to provide a container for the supplied
// methods.

class ClassTable
{
private:
  InheritanceGraph *inheritance_graph;
  std::map<Symbol, InheritanceGraphNode *> *class_map; // Maintain a map to speed up the search

  int semant_errors;
  void install_basic_classes();
  ostream &error_stream;

public:
  ClassTable(Classes);
  int errors() { return semant_errors; }
  ostream &semant_error();
  ostream &semant_error(Class_ c);
  ostream &semant_error(Symbol filename, tree_node *t);
  // Inheritance graph check
  int check_inheritance_graph(Classes classes);
  // Anaylsis errors in the nonempty free_node_set
  void analysis_inheritance_error();
};

#endif