

#include <stdlib.h>
#include <stdio.h>
#include <stdarg.h>
#include "semant.h"
#include "utilities.h"

extern int semant_debug;
extern char *curr_filename;
//////////////////////////////////////////////////////////////////////
//
// Symbols
//
// For convenience, a large number of symbols are predefined here.
// These symbols include the primitive type and method names, as well
// as fixed names used by the runtime system.
//
//////////////////////////////////////////////////////////////////////
static Symbol
    arg,
    arg2,
    Bool,
    concat,
    cool_abort,
    copy,
    Int,
    in_int,
    in_string,
    IO,
    length,
    Main,
    main_meth,
    No_class,
    No_type,
    Object,
    out_int,
    out_string,
    prim_slot,
    self,
    SELF_TYPE,
    Str,
    str_field,
    substr,
    type_name,
    val;
//
// Initializing the predefined symbols.
//
static void initialize_constants(void)
{
  arg = idtable.add_string("arg");
  arg2 = idtable.add_string("arg2");
  Bool = idtable.add_string("Bool");
  concat = idtable.add_string("concat");
  cool_abort = idtable.add_string("abort");
  copy = idtable.add_string("copy");
  Int = idtable.add_string("Int");
  in_int = idtable.add_string("in_int");
  in_string = idtable.add_string("in_string");
  IO = idtable.add_string("IO");
  length = idtable.add_string("length");
  Main = idtable.add_string("Main");
  main_meth = idtable.add_string("main");
  //   _no_class is a symbol that can't be the name of any
  //   user-defined class.
  No_class = idtable.add_string("_no_class");
  No_type = idtable.add_string("_no_type");
  Object = idtable.add_string("Object");
  out_int = idtable.add_string("out_int");
  out_string = idtable.add_string("out_string");
  prim_slot = idtable.add_string("_prim_slot");
  self = idtable.add_string("self");
  SELF_TYPE = idtable.add_string("SELF_TYPE");
  Str = idtable.add_string("String");
  str_field = idtable.add_string("_str_field");
  substr = idtable.add_string("substr");
  type_name = idtable.add_string("type_name");
  val = idtable.add_string("_val");
}

// Load classes in program into free_node_set
int InheritanceGraph::add_free_node(Class_ c)
{
  InheritanceGraphNode *node = new InheritanceGraphNode(c);
  if (free_node_set->find(node) != free_node_set->end()) // indicate that the class has been alredy exist
    return 1;
  free_node_set->insert(node);
  return 0;
}

// Build inheritance tree and leave classes with wrong inheritance into free_node_set
int InheritanceGraph::construct_tree()
{
  // assert there is no duplicate class
  std::map<Symbol, InheritanceGraphNode *> *leaf_map = new std::map<Symbol, InheritanceGraphNode *>;
  std::map<Symbol, InheritanceGraphNode *> *new_leaf_map = new std::map<Symbol, InheritanceGraphNode *>;
  std::map<Symbol, InheritanceGraphNode *> *temp_ptr = NULL; // use in switch

  // Set class Object as the first leaf of inheritance tree
  leaf_map->insert(std::pair<Symbol, InheritanceGraphNode *>(Object, root));
  while (true)
  {
    new_leaf_map->clear();
    for (InheritanceGraphNode *cur_free_node : *free_node_set)
    {
      // Find cur_free_node's parent on the tree
      auto it = leaf_map->find(cur_free_node->getParentName());
      if (it != leaf_map->end())
      {
        cur_free_node->setParent(it->second);
        new_leaf_map->insert(std::pair<Symbol, InheritanceGraphNode *>(cur_free_node->getName(), cur_free_node));
      }
    }
    if (new_leaf_map->size() == 0) // Stop when there is no new leaf added after one iteration.
      break;
    // Assert new_leaf_map.size() > 0
    // Clean up removed nodes in free_node_set
    for (std::pair<Symbol, InheritanceGraphNode *> pair : *new_leaf_map)
    {
      auto free_node_it = free_node_set->find(pair.second);
      if (free_node_it != free_node_set->end())
        free_node_set->erase(free_node_it);
    }
    // Switch two maps
    temp_ptr = leaf_map;
    leaf_map = new_leaf_map;
    new_leaf_map = temp_ptr;
  }

  if (free_node_set->size() != 0) 
    // indicate that there have classe(s) with non-defined parent
    // or cyclic inheritance(s) exist(s)
    return 1;
  return 0;
}

// Constructor of ClassTable.
ClassTable::ClassTable(Classes classes) : semant_errors(0), error_stream(cerr)
{
  this->inheritance_graph = new InheritanceGraph();

  // Install all the classes and pre-defined classes in to list.
  this->install_basic_classes();
  /* Fill this in */
  // Conduct the Inheritance checking
  this->check_inheritance_graph(classes);
}

int ClassTable::check_inheritance_graph(Classes classes)
{
  bool contain_main = false;
  // Load classes into free_node_set, check dupilcate define
  for (int i = classes->first(); classes->more(i); i = classes->next(i))
  {
    Class_ cur_class = classes->nth(i);
    Symbol name = cur_class->getName();
    Symbol parent_name = cur_class->getParentName();
    if (name == Main)
      contain_main = true;
    if (name == Object || name == IO || name == Int || name == Str || name == Bool)
    {
      semant_error(cur_class) << "Cannot redefine basic class: " << name;
      return 1;
    }
    if (parent_name == IO || parent_name == Int || parent_name == Str || parent_name == Bool || parent_name == SELF_TYPE)
    {
      semant_error(cur_class) << "Cannot inherit from basic class: " << parent_name;
      return 1;
    }
    // Load cur_class to free_node_set, check duplicating defined, as well.
    if (inheritance_graph->add_free_node(cur_class) == 1)
    {
      semant_error(cur_class) << "class: " << name << " is already defined";
      return 1;
    }
    // Load cur_class to class_map.
    class_map->insert(std::pair<Symbol, InheritanceGraphNode *>(name, new InheritanceGraphNode(cur_class)));
  }
  if (!contain_main)
  {
    semant_error() << "No main class";
    return 1;
  }
  // Construct graph based on free_node_set, check the rest of rules
  if (inheritance_graph->construct_tree() != 1)
    return 0;
  analysis_inheritance_error();
  return 1;
}

// Anaylsis errors between classes left in free_node_set
// Generate error message and report
void ClassTable::analysis_inheritance_error()
{
  InheritanceGraphNode *ptr = NULL;
  std::set<InheritanceGraphNode *> *free_node_set = inheritance_graph->getFreeNodeSet();
  std::set<InheritanceGraphNode *> *temp_node_set = new std::set<InheritanceGraphNode *>();
  // Find out undefined-parent inheritance
  while (free_node_set->size() != 0)
  {
    ptr = *(free_node_set->begin());
    std::set<InheritanceGraphNode *>::iterator it;
    // Construct error message
    std::string err_msg = strcat("  ->  class ", ptr->getName()->get_string()); // "  ->  class xxx"
    temp_node_set->insert(ptr);
    // Collect all the ancestors of the current checking class and detect cyclic inheritance
    while ((it = free_node_set->find((*class_map)[ptr->getParentName()])) != free_node_set->end())
    {
      ptr = *it;
      err_msg.insert(0, strcat("  ->  class ", ptr->getName()->get_string())); // "  ->  class parent  ->  class child"
      temp_node_set->insert(ptr);
      if ((it = temp_node_set->find((*class_map)[ptr->getParentName()])) != temp_node_set->end())
      {
        ptr = *it;
        // ">> Class child <<  ->  Class parent  ->  Class child"
        err_msg.insert(0, strcat(strcat("Cyclic Inheritance exists: \n>> class ", ptr->getName()->get_string()), " <<"));
        break;
      }
    }
    if (it == free_node_set->end()) // Find undefined parent
      err_msg.insert(0, strcat(strcat("Undefined parent class: ", ptr->getParentName()->get_string()), "\n?"));
    semant_error(ptr->getClass()) << err_msg;

    // Clean up checked classes
    free_node_set->erase(temp_node_set->begin(), temp_node_set->end());
  }
}

/*----------------------------------------------------------------------------------.
| ClassTable::install_basic_classes()                                               |
|  Initialize build-in classes of COOL, which can be directly used by programmers.  |
`__________________________________________________________________________________*/
void ClassTable::install_basic_classes()
{

  // The tree package uses these globals to annotate the classes built below.
  // curr_lineno  = 0;
  Symbol filename = stringtable.add_string("<basic class>");

  // The following demonstrates how to create dummy parse trees to
  // refer to basic Cool classes.  There's no need for method
  // bodies -- these are already built into the runtime system.

  // IMPORTANT: The results of the following expressions are
  // stored in local variables.  You will want to do something
  // with those variables at the end of this method to make this
  // code meaningful.

  //
  // The Object class has no parent class. Its methods are
  //        abort() : Object    aborts the program
  //        type_name() : Str   returns a string representation of class name
  //        copy() : SELF_TYPE  returns a copy of the object
  //
  // There is no need for method bodies in the basic classes---these
  // are already built in to the runtime system.

  Class_ Object_class =
      class_(Object,
             No_class,
             append_Features(
                 append_Features(
                     single_Features(method(cool_abort, nil_Formals(), Object, no_expr())),
                     single_Features(method(type_name, nil_Formals(), Str, no_expr()))),
                 single_Features(method(copy, nil_Formals(), SELF_TYPE, no_expr()))),
             filename);

  //
  // The IO class inherits from Object. Its methods are
  //        out_string(Str) : SELF_TYPE       writes a string to the output
  //        out_int(Int) : SELF_TYPE            "    an int    "  "     "
  //        in_string() : Str                 reads a string from the input
  //        in_int() : Int                      "   an int     "  "     "
  //
  Class_ IO_class =
      class_(IO,
             Object,
             append_Features(
                 append_Features(
                     append_Features(
                         single_Features(method(out_string, single_Formals(formal(arg, Str)),
                                                SELF_TYPE, no_expr())),
                         single_Features(method(out_int, single_Formals(formal(arg, Int)),
                                                SELF_TYPE, no_expr()))),
                     single_Features(method(in_string, nil_Formals(), Str, no_expr()))),
                 single_Features(method(in_int, nil_Formals(), Int, no_expr()))),
             filename);

  //
  // The Int class has no methods and only a single attribute, the
  // "val" for the integer.
  //
  Class_ Int_class =
      class_(Int,
             Object,
             single_Features(attr(val, prim_slot, no_expr())),
             filename);

  //
  // Bool also has only the "val" slot.
  //
  Class_ Bool_class =
      class_(Bool, Object, single_Features(attr(val, prim_slot, no_expr())), filename);

  //
  // The class Str has a number of slots and operations:
  //       val                                  the length of the string
  //       str_field                            the string itself
  //       length() : Int                       returns length of the string
  //       concat(arg: Str) : Str               performs string concatenation
  //       substr(arg: Int, arg2: Int): Str     substring selection
  //
  Class_ Str_class =
      class_(Str,
             Object,
             append_Features(
                 append_Features(
                     append_Features(
                         append_Features(
                             single_Features(attr(val, Int, no_expr())),
                             single_Features(attr(str_field, prim_slot, no_expr()))),
                         single_Features(method(length, nil_Formals(), Int, no_expr()))),
                     single_Features(method(concat,
                                            single_Formals(formal(arg, Str)),
                                            Str,
                                            no_expr()))),
                 single_Features(method(substr,
                                        append_Formals(single_Formals(formal(arg, Int)),
                                                       single_Formals(formal(arg2, Int))),
                                        Str,
                                        no_expr()))),
             filename);

  InheritanceGraphNode *Object_node = new InheritanceGraphNode(Object_class);
  this->inheritance_graph->setRoot(Object_node); // Object class is the root of the inheritance graph.
  InheritanceGraphNode *IO_node = new InheritanceGraphNode(IO_class);
  IO_node->setParent(Object_node);
  InheritanceGraphNode *Int_node = new InheritanceGraphNode(Int_class);
  Int_node->setParent(Object_node);
  InheritanceGraphNode *Bool_node = new InheritanceGraphNode(Bool_class);
  Bool_node->setParent(Object_node);
  InheritanceGraphNode *Str_node = new InheritanceGraphNode(Str_class);
  Str_node->setParent(Object_node);
}

////////////////////////////////////////////////////////////////////
//
// semant_error is an overloaded function for reporting errors
// during semantic analysis.  There are three versions:
//
//    ostream& ClassTable::semant_error()
//
//    ostream& ClassTable::semant_error(Class_ c)
//       print line number and filename for `c'
//
//    ostream& ClassTable::semant_error(Symbol filename, tree_node *t)
//       print a line number and filename
//
///////////////////////////////////////////////////////////////////

ostream &ClassTable::semant_error(Class_ c)
{
  return semant_error(c->get_filename(), c);
}

ostream &ClassTable::semant_error(Symbol filename, tree_node *t)
{
  error_stream << filename << ":" << t->get_line_number() << ": ";
  return semant_error();
}

ostream &ClassTable::semant_error()
{
  semant_errors++;
  return error_stream;
}

/*   This is the entry point to the semantic checker.

   Your checker should do the following two things:

   1) Check that the program is semantically correct
   2) Decorate the abstract syntax tree with type information
      by setting the `type' field in each Expression node.
      (see `tree.h')

   You are free to first do 1), make sure you catch all semantic
   errors. Part 2) can be done in a second stage, when you want
   to build mycoolc.
 */
void program_class::semant()
{
  initialize_constants();

  /* ClassTable constructor may do some semantic analysis */
  // Here, we decide to conduct Inheritance checking first, inside the ClassTable constructor.
  ClassTable *classtable = new ClassTable(classes);

  /* some semantic analysis code may go here */

  if (classtable->errors())
  {
    cerr << "Compilation halted due to static semantic errors." << endl;
    exit(1);
  }
}