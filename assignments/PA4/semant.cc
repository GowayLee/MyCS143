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
    val,
    ERR_type; //  When an error occurs during type-checking, the type of the current node is ERR_type.
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
  ERR_type = idtable.add_string("ERR_type");
}

// Constructor of ClassTable.
ClassTable::ClassTable(Classes classes)
{
  this->inheritance_graph = new InheritanceGraph();
  // Install all the classes and pre-defined classes in to list.
  this->install_basic_classes();
  // Conduct the Inheritance checking
  this->check_inheritance_graph(classes);
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
  delete new_leaf_map, leaf_map;

  if (free_node_set->size() != 0)
    // indicate that there have classe(s) with non-defined parent
    // or cyclic inheritance(s) exist(s)
    return 1;
  return 0;
}

void ClassTable::check_inheritance_graph(Classes classes)
{
  // Load classes into free_node_set, check dupilcate define
  for (int i = classes->first(); classes->more(i); i = classes->next(i))
  {
    Class_ cur_class = classes->nth(i);
    Symbol name = cur_class->getName();
    Symbol parent_name = cur_class->getParentName();
    if (name == Object || name == IO || name == Int || name == Str || name == Bool)
    {
      semant_error(cur_class) << "Cannot redefine basic class: " << name << endl;
      continue;
    }
    if (parent_name == Int || parent_name == Str || parent_name == Bool || parent_name == SELF_TYPE)
    {
      semant_error(cur_class) << "Cannot inherit from basic class: " << parent_name << endl;
      continue;
    }
    // Load cur_class to free_node_set, check duplicating defined, as well.
    if (inheritance_graph->add_free_node(cur_class) == 1)
    {
      semant_error(cur_class) << "Duplicate definition of class " << name << "." << endl;
      continue;
    }
    // Load cur_class to class_map.
    Env::class_map->insert(std::pair<Symbol, InheritanceGraphNode *>(name, new InheritanceGraphNode(cur_class)));
  }
  // Construct graph based on free_node_set, check the rest of rules
  if (inheritance_graph->construct_tree())
    analysis_inheritance_error();
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
    while ((it = free_node_set->find(ptr->getParent())) != free_node_set->end())
    {
      ptr = *it;
      err_msg.insert(0, strcat("  ->  class ", ptr->getName()->get_string())); // "  ->  class parent  ->  class child"
      temp_node_set->insert(ptr);
      if ((it = temp_node_set->find(ptr->getParent())) != temp_node_set->end())
      {
        ptr = *it;
        // ">> Class child <<  ->  Class parent  ->  Class child"
        err_msg.insert(0, strcat(strcat("Cyclic Inheritance exists: \n>> class ", ptr->getName()->get_string()), " <<"));
        break;
      }
    }
    if (it == free_node_set->end()) // Find undefined parent
    {
      err_msg.insert(0, strcat(strcat("Undefined parent class: ", ptr->getParentName()->get_string()), "\n?"));
    }
    semant_error(ptr->getClass()) << err_msg << endl;

    // Clean up checked classes
    free_node_set->erase(temp_node_set->begin(), temp_node_set->end());
  }
  delete temp_node_set;
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
  Env::class_map->insert(std::pair<Symbol, InheritanceGraphNode *>(Object, Object_node));

  InheritanceGraphNode *IO_node = new InheritanceGraphNode(IO_class);
  IO_node->setParent(Object_node);
  Env::class_map->insert(std::pair<Symbol, InheritanceGraphNode *>(IO, IO_node));

  InheritanceGraphNode *Int_node = new InheritanceGraphNode(Int_class);
  Int_node->setParent(Object_node);
  Env::class_map->insert(std::pair<Symbol, InheritanceGraphNode *>(Int, Int_node));

  InheritanceGraphNode *Bool_node = new InheritanceGraphNode(Bool_class);
  Bool_node->setParent(Object_node);
  Env::class_map->insert(std::pair<Symbol, InheritanceGraphNode *>(Bool, Bool_node));

  InheritanceGraphNode *Str_node = new InheritanceGraphNode(Str_class);
  Str_node->setParent(Object_node);
  Env::class_map->insert(std::pair<Symbol, InheritanceGraphNode *>(Str, Str_node));
}

// Install methods into method_env and check main class and main method.
// Check duplicate definition of method in one class.
// In this period, we don't check the conrrectness of inheritance and override.
int ClassTable::install_features(Classes classes)
{
  bool check_main_class = false;
  bool check_main_method = false;
  std::set<Symbol> *method_set = new std::set<Symbol>(); // Maintain a set to detect duplicate definition of method in one class
  std::set<Symbol> *attr_set = new std::set<Symbol>();   // Detect dupilicate definition of attribute in one class

  for (int i = classes->first(); classes->more(i); i = classes->next(i))
  {
    method_set->clear();
    attr_set->clear();

    Class_ curr_class = classes->nth(i);
    if (!check_main_class && curr_class->getName() == Main)
      check_main_class = true;
    Features features = curr_class->getFeatures();
    for (int j = features->first(); features->more(j); j = features->next(j))
    {
      if (features->nth(j)->isMethod())
      {
        method_class *curr_method = static_cast<method_class *>(features->nth(j));
        if (method_set->find(curr_method->getName()) != method_set->end())
        {
          semant_error(curr_class) << "Duplicate definition of method " << curr_method->getName() << "." << endl;
          continue;
        }
        if (check_main_class && !check_main_method && curr_method->getName() == main_meth)
          check_main_method = true;

        Formals formals = curr_method->getFormals();
        std::vector<Symbol> *arg_types = new std::vector<Symbol>();
        // Insert arguments' types
        for (int k = formals->first(); formals->more(k); k = formals->next(k))
          arg_types->push_back(formals->nth(k)->getType());
        // Insert return type
        arg_types->push_back(curr_method->getReturnType());
        // Insert into method_env
        Env::method_map->insert(std::make_pair(std::make_pair(curr_class->getName(), curr_method->getName()), arg_types));
      }
      else
      {
        attr_class *curr_attr = static_cast<attr_class *>(features->nth(j));
        if (attr_set->find(curr_attr->getName()) != attr_set->end())
        {
          semant_error(curr_class) << "Duplicate definition of attribute " << curr_attr->getName() << "." << endl;
          continue;
        }
        // Insert into object_env
        Env::attr_map->insert(std::make_pair(std::make_pair(curr_class->getName(), curr_attr->getName()), curr_attr->getType()));
      }
    }
  }
  delete method_set, attr_set;

  if (!check_main_class)
    semant_error() << "Main class is missing." << endl;
  else if (!check_main_method)
    semant_error() << "Main method is missing." << endl;
  else
    return 0;
  return 1;
}

// Get all ancestors of the current class
// Return a List containing all ancestors and current class
List<Class__class> *ClassTable::get_ancestors(Class_ cur_class)
{
  // Initialize List and append cur_class
  List<Class__class> *ancestors = new List<Class__class>(cur_class, (List<Class__class> *)NULL);
  InheritanceGraphNode *cur_node = (*Env::class_map)[cur_class->getName()];
  while (cur_node->getParent()) // Stop when reach root
    ancestors = new List<Class__class>((cur_node = cur_node->getParent())->getClass(), ancestors);
  return ancestors;
}

int ClassTable::setup_environment(Class_ cur_class)
{
  int state = 0;
  // Initalize object_env and method_env
  // Memory free done in outer scope(in program_class::type_check())
  Env::object_env = new SymbolTable<Symbol, Entry>();
  Env::method_env = new SymbolTable<Symbol, std::vector<Symbol>>();
  // Set up type environments
  List<Class__class> *ancestors = get_ancestors(cur_class);
  Env::object_env->enterscope();
  Env::method_env->enterscope();
  Env::cur_class = cur_class;
  // Traverse ancestors from root to current class, install features and check override
  for (List<Class__class> *i = ancestors; i->tl(); i = i->tl())
  {
    Class_ cur_ancestor = i->hd();
    for (int j = cur_ancestor->getFeatures()->first(); cur_ancestor->getFeatures()->more(j); j = cur_ancestor->getFeatures()->next(j))
    {
      if (cur_ancestor->getFeatures()->nth(j)->isMethod())
      {
        method_class *curr_method = static_cast<method_class *>(cur_ancestor->getFeatures()->nth(j));
        Formals formals = curr_method->getFormals();
        std::vector<Symbol> *arg_types = new std::vector<Symbol>();
        // Insert arguments' types
        for (int k = formals->first(); formals->more(k); k = formals->next(k))
          arg_types->push_back(formals->nth(k)->getType());
        // Insert return type
        arg_types->push_back(curr_method->getReturnType());

        // Same method name exists in ancestor, check override
        if (std::vector<Symbol> *p_arg_types = Env::method_env->lookup(curr_method->getName()))
        {
          // check the number of arguments
          if (p_arg_types->size() != arg_types->size())
          {
            semant_error(cur_class) << "Method " << curr_method->getName() << " has different number of arguments with signature super class." << endl;
            Env::method_map->erase(std::make_pair(cur_class->getName(), curr_method->getName())); // remove current method
            state = 1;
            continue;
          }
          // check the type of arguments
          for (long unsigned int k = 0; k < p_arg_types->size(); k++)
          {
            if ((*p_arg_types)[k] != (*arg_types)[k])
            {
              semant_error(cur_class) << "Method " << curr_method->getName() << " has different type of arguments with signature super class." << endl;
              Env::method_map->erase(std::make_pair(cur_class->getName(), curr_method->getName())); // remove current method
              state = 1;
              continue;
            }
          }
          // legal override
          continue;
        }
        Env::method_env->addid(curr_method->getName(), arg_types);
        delete arg_types;
      }
      else
      {
        attr_class *curr_attr = static_cast<attr_class *>(cur_ancestor->getFeatures()->nth(j));
        if (Env::object_env->lookup(curr_attr->getName())) // Duplicate define attributes
        {
          semant_error(cur_class) << "Attribute " << curr_attr->getName() << " is already defined in super class" << endl;
          Env::attr_map->erase(std::make_pair(cur_class->getName(), curr_attr->getName())); // remove current attribute
          state = 1;
          continue;
        }
        Env::object_env->addid(curr_attr->getName(), curr_attr->getType());
      }
    }
  }
  delete ancestors;
  return state;
}

// Return true if type2 <= type1
bool ClassTable::check_subtype(Symbol type1, Symbol type2)
{
  if (type1 == No_type && type2 == No_type)
    return true; // filter out No_type
  if (type1 == No_type || type2 == No_type)
    return false;
  // Check existence of both types
  std::map<Symbol, InheritanceGraphNode *>::iterator it;
  if ((it = Env::class_map->find(type1)) == Env::class_map->end()) // Cannot find type1 in class_map
  {
    semant_error(Env::cur_class) << "Type " << type1 << " is undeclared." << endl;
    return false;
  }
  if ((it = Env::class_map->find(type2)) == Env::class_map->end()) // Cannot find type2 in class_map
  {
    semant_error(Env::cur_class) << "Type " << type2 << " is undeclared." << endl;
    return false;
  }

  // Check subtyping
  if (type1 == Object)
    return true;

  InheritanceGraphNode *ptr = it->second;
  while (ptr)
  {
    if (ptr->getName() == type1)
      return true;
    ptr = ptr->getParent();
  }
  return false;
}

Symbol ClassTable::get_LCA(std::vector<Symbol> *type_list)
{
  if (type_list->size() == 1)
    return (*type_list)[0];
  Symbol prev_type = (*type_list)[0];
  for (Symbol cur_type : *type_list)
    prev_type = get_LCA(prev_type, cur_type);
  return prev_type;
}

// Return LCA type of type1 and type2
Symbol ClassTable::get_LCA(Symbol type1, Symbol type2)
{
  if (type1 == No_type || type2 == No_type) // LCA of No_type and any type is No_type
    return No_type;

  // check the existence of both types
  std::map<Symbol, InheritanceGraphNode *>::iterator it1;
  std::map<Symbol, InheritanceGraphNode *>::iterator it2;
  if ((it1 = Env::class_map->find(type1)) == Env::class_map->end()) // Cannot find type1 in class_map
  {
    semant_error(Env::cur_class) << "Type " << type1 << " is undeclared." << endl;
    return ERR_type;
  }
  if ((it2 = Env::class_map->find(type2)) == Env::class_map->end()) // Cannot find type2 in class_map
  {
    semant_error(Env::cur_class) << "Type " << type2 << " is undeclared." << endl;
    return ERR_type;
  }

  // Find LCA
  List<Class__class> *path1 = new List<Class__class>(it1->second->getClass(), (List<Class__class> *)NULL);
  // Generate a path from type1 to Object and check subtyping of type1 and type2 at the same time
  for(InheritanceGraphNode *ptr = it1->second; ptr->getParent(); ptr = ptr->getParent())
  {
    if (ptr == it2->second) // Indicate that *ptr is equal to or a subtype of type2
    {
      delete path1;
      return ptr->getName();
    }
    path1 = new List<Class__class>(ptr->getClass(), path1);
  }
  List<Class__class> *path2 = new List<Class__class>(it2->second->getClass(), (List<Class__class> *)NULL);
  for(InheritanceGraphNode *ptr = it2->second; ptr->getParent(); ptr = ptr->getParent())
  {
    if (ptr == it1->second) // Indicate that *ptr is equal to or a subtype of type1
    {
      delete path1, path2;
      return ptr->getName();
    }
    path2 = new List<Class__class>(ptr->getClass(), path2);
  }
  // type1 and type2 are on the different path
  for (List<Class__class> *ptr1 = path1, *ptr2 = path2; ptr1->tl()->tl() && ptr2->tl()->tl(); ptr1 = ptr1->tl(), ptr2 = ptr2->tl())
    if (ptr1->tl()->hd() != ptr2->tl()->hd())
    {
      Symbol result = ptr1->hd()->getName();
      delete path1, path2; // Need to delte from head of the List
      return result;
    }
  return ERR_type; // Actually, this line should never be reached
}


void program_class::type_check()
{
  // Set up type environments
  // 1. Install methods of all classes into the environment
  if (ClassTable::install_features(classes)) // Missing main class or main method
    return;

  // 2. Traverse each class and do type checking, maintain the object_env at the same time.
  for (int i = classes->first(); classes->more(i); i = classes->next(i))
  {
    if (ClassTable::setup_environment(classes->nth(i)))
      continue;
    classes->nth(i)->type_check(); // Traverse tree and get all nodes type checked
    delete Env::object_env, Env::method_env;
  }
}

void class__class::type_check()
{
  for (int i = getFeatures()->first(); getFeatures()->more(i); i = getFeatures()->next(i))
  {
    if (getFeatures()->nth(i)->isMethod())
      static_cast<method_class *>(getFeatures()->nth(i))->type_check();
    else
      static_cast<attr_class *>(getFeatures()->nth(i))->type_check();
  }
}

void attr_class::type_check()
{
  Symbol attr_type = (getType() == SELF_TYPE) ? Env::cur_class->getName() : getType();

  Env::object_env->enterscope(); // Set up a new scope with self = SELF_TYPE
  Env::object_env->addid(self, Env::cur_class->getName());
  init->type_check(); // Check the type of init expression
  Env::object_env->exitscope();

  if (init->get_type() == ERR_type)
    return; // If init is ERR_type, just return
  if ((init->get_type() != No_type) && !ClassTable::check_subtype(attr_type, init->get_type()))
  {
    ClassTable::semant_error(Env::cur_class) << "Type " << init->get_type() << " is not equal to or a subtype of " << attr_type << endl;
    return;
  }
  
  // Add new attribute to object_env
  Env::object_env->addid(name, type_decl);
}

void method_class::type_check()
{
  // Assert all the methods have been installed into the method_map
  std::vector<Symbol> *arg_types = Env::method_map->find(std::make_pair(Env::cur_class->getName(), name))->second;
  
  // Set up a new scope with self and all the arguments
  Env::object_env->enterscope();
  Env::object_env->addid(self, Env::cur_class->getName());
  for (int i = formals->first(); formals->more(i); i = formals->next(i))
    Env::object_env->addid(formals->nth(i)->getName(), formals->nth(i)->getType());
  expr->type_check();
  Env::object_env->exitscope();

  if (expr->get_type() == ERR_type)
    return; // If expr is ERR_type, just return
  if (!ClassTable::check_subtype(arg_types->back(), expr->get_type()))
    ClassTable::semant_error(Env::cur_class) << "Type " << expr->get_type() << " is not equal to or a subtype of " << arg_types->back() << endl;
}

/////////////////////////////////////////////////////////////////
// Start type-checking on all sub-classes of expression_class //
////////////////////////////////////////////////////////////////
void object_class::type_check()
{
  if (Symbol result = Env::object_env->lookup(name)) // check context in object identifier environment
    set_type(result);
  else
  {
    ClassTable::semant_error(Env::cur_class) << "Undeclared identifier " << name << endl;
    set_type(ERR_type);
  }
}

void assign_class::type_check()
{
  Symbol id_type = Env::object_env->lookup(name);
  if (!id_type)
  {
    ClassTable::semant_error(Env::cur_class) << "Undeclared identifier " << name << endl;
    set_type(ERR_type);
    return;
  }
  // TODO: Can assign to identifier `self` ?

  Env::object_env->enterscope();
  expr->type_check();
  Env::object_env->exitscope();

  if (expr->get_type() == ERR_type) // If expr is ERR_type, the whole assign_class is ERR_type
  {
    set_type(ERR_type);
    return;
  }
  if (expr->get_type() == No_type) // Cannot assign No_type
  {
    ClassTable::semant_error(Env::cur_class) << "Cannot assign No_type to " << name << endl;
    set_type(ERR_type);
    return;
  }
  if (!ClassTable::check_subtype(id_type, expr->get_type()))
  {
    ClassTable::semant_error(Env::cur_class) << "Type " << expr->get_type() << " is not equal to or a subtype of " << id_type << endl;
    set_type(ERR_type);
  }
  set_type(expr->get_type());
}

void bool_const_class::type_check() { set_type(Bool); }

void int_const_class::type_check() { set_type(Int); }

void string_const_class::type_check() { set_type(Str); }

void new__class::type_check()
{
  if (type_name == SELF_TYPE)
  {
    set_type(Env::cur_class->getName());
    return;
  }

  if (Env::class_map->find(type_name) != Env::class_map->end())
    set_type(type_name);
  else
  {
    ClassTable::semant_error(Env::cur_class) << "Undeclared type " << type_name << endl;
    set_type(ERR_type);
  }
  return;
}

void dispatch_class::type_check()
{
  // type_check of e0
  Env::object_env->enterscope();
  expr->type_check();
  Env::object_env->exitscope();

  Symbol e0_type = expr->get_type() == SELF_TYPE ? Env::cur_class->getName() : expr->get_type();
  if (e0_type == ERR_type) // If e0 is ERR_type, the whole dispatch_class is ERR_type
  {
    set_type(ERR_type);
    return;
  }
  else if (e0_type == No_type) // Cannot dispatch on No_type
  {
    ClassTable::semant_error(Env::cur_class) << "Cannot dispatch on No_type" << endl;
    set_type(ERR_type);
    return;
  }

  // type_check of the dispatched method
  std::map<std::pair<Symbol, Symbol>, std::vector<Symbol> *>::iterator it = Env::method_map->find(std::make_pair(e0_type, name));
  if (it == Env::method_map->end())
  {
    ClassTable::semant_error(Env::cur_class) << "Undeclared method " << name << " in class " << e0_type << endl;
    set_type(ERR_type);
    return;
  }
  // type_check of the actual arguments
  std::vector<Symbol> *formal_types = it->second;
  if (formal_types->size() - 1 != (long unsigned int)actual->len())
  {
    ClassTable::semant_error(Env::cur_class) << "Wrong number of arguments for method " << name << " in class " << e0_type << endl;
    set_type(ERR_type);
    return;
  }
  for (int i = actual->first(); actual->more(i); i = actual->next(i))
  {
    Env::object_env->enterscope();
    actual->nth(i)->type_check();
    Env::object_env->exitscope();

    if (actual->nth(i)->get_type() == ERR_type)
    {
      set_type(ERR_type);
      return;
    }
    if (!ClassTable::check_subtype(formal_types->at(i), actual->nth(i)->get_type()))
    {
      ClassTable::semant_error(Env::cur_class) << "Type " << actual->nth(i)->get_type() << " is not equal to or a subtype of " << formal_types->at(i) << endl;
      set_type(ERR_type);
      return;
    }
  }

  // type_check of return type
  Symbol return_type = formal_types->back();
  set_type(return_type == SELF_TYPE ? e0_type : return_type);
  return;
}

void static_dispatch_class::type_check()
{
  // type_check of e0 and T
  Env::object_env->enterscope();
  expr->type_check();
  Env::object_env->exitscope();

  Symbol e0_type = expr->get_type();
  Symbol T_type = type_name;
  if (e0_type == ERR_type) // If e0 is ERR_type, the whole dispatch_class is ERR_type
  {
    set_type(ERR_type);
    return;
  }
  else if (e0_type == No_type || T_type == No_type) // Cannot dispatch on No_type
  {
    ClassTable::semant_error(Env::cur_class) << "Cannot dispatch on No_type" << endl;
    set_type(ERR_type);
    return;
  }
  // check subtyping of e0 and T
  if (!ClassTable::check_subtype(T_type, e0_type))
  {
    ClassTable::semant_error(Env::cur_class) << "Type " << e0_type << " is not equal to or a subtype of " << T_type << endl;
    set_type(ERR_type);
    return;
  }

  // type_check of the dispatched method
  std::map<std::pair<Symbol, Symbol>, std::vector<Symbol> *>::iterator it = Env::method_map->find(std::make_pair(T_type, name));
  if (it == Env::method_map->end())
  {
    ClassTable::semant_error(Env::cur_class) << "Undeclared method " << name << " in class " << T_type << endl;
    set_type(ERR_type);
    return;
  }
  // type_check of the actual arguments
  std::vector<Symbol> *formal_types = it->second;
  if (formal_types->size() - 1 != (long unsigned int)actual->len())
  {
    ClassTable::semant_error(Env::cur_class) << "Wrong number of arguments for method " << name << " in class " << T_type << endl;
    set_type(ERR_type);
    return;
  }
  for (int i = actual->first(); actual->more(i); i = actual->next(i))
  {
    Env::object_env->enterscope();
    actual->nth(i)->type_check();
    Env::object_env->exitscope();

    if (actual->nth(i)->get_type() == ERR_type)
    {
      set_type(ERR_type);
      return;
    }
    if (!ClassTable::check_subtype(formal_types->at(i), actual->nth(i)->get_type()))
    {
      ClassTable::semant_error(Env::cur_class) << "Type " << actual->nth(i)->get_type() << " is not equal to or a subtype of " << formal_types->at(i) << endl;
      set_type(ERR_type);
      return;
    }
  }

  // type_check of return type
  Symbol return_type = formal_types->back();
  set_type(return_type == SELF_TYPE ? e0_type : return_type);
  return;
}

void cond_class::type_check()
{
  Env::object_env->enterscope();
  pred->type_check();
  Env::object_env->exitscope();

  if (pred->get_type() != Bool)
  {
    ClassTable::semant_error(Env::cur_class) << "COND expression of If statement is not a bool" << endl;
    set_type(ERR_type);
    return;
  }

  Env::object_env->enterscope();
  then_exp->type_check();
  Env::object_env->exitscope();

  Env::object_env->enterscope();
  else_exp->type_check();
  Env::object_env->exitscope();

  if (then_exp->get_type() == ERR_type || else_exp->get_type() == ERR_type)
  {
    set_type(ERR_type);
    return;
  }
  if (then_exp->get_type() == No_type || else_exp->get_type() == No_type)
  {
    ClassTable::semant_error(Env::cur_class) << "THEN or ELSE expression cannot be No_type in If statement" << endl;
    set_type(ERR_type);
    return;
  }
  set_type(ClassTable::get_LCA(then_exp->get_type(), else_exp->get_type()));
}

void block_class::type_check()
{
  int i = body->first();
  Expression last_expr;
  for (i; body->more(i); i = body->next(i)) // do type-checking for all expressions in the block
  {
    Env::object_env->enterscope();
    last_expr = body->nth(i);
    last_expr->type_check();
    Env::object_env->exitscope();
  }

  if (last_expr->get_type() == ERR_type)
  {
    set_type(ERR_type);
    return;
  }
  set_type(last_expr->get_type());
}

void let_class::type_check()
{
  // do type-checking for identifier
  if (type_decl == No_type)
  {
    ClassTable::semant_error(Env::cur_class) << "Type of variable " << identifier << " is not declared" << endl;
    set_type(ERR_type);
    return;
  }
  Symbol id_type;
  if (type_decl == SELF_TYPE)
    id_type = Env::cur_class->getName();
  
  // do type-checking for init
  Env::object_env->enterscope();
  init->type_check();
  Env::object_env->exitscope();

  Symbol init_type = init->get_type();
  if (init_type == ERR_type)
  {
    set_type(ERR_type);
    return;
  }
  if (init_type != No_type) // in Let-No-Init, init is No_type, no need to check subtype
  {
    if (!ClassTable::check_subtype(id_type, init_type))
    {
      ClassTable::semant_error(Env::cur_class) << "Type " << init_type << " is not equal to or a subtype of " << id_type << endl;
      set_type(ERR_type);
      return;
    }
  }

  // add identifier to object environment
  Env::object_env->enterscope();
  Env::object_env->addid(identifier, id_type);
  body->type_check();
  Env::object_env->exitscope();

  set_type(body->get_type());
}

void typcase_class::type_check()
{
  // do type-checking for expr
  Env::object_env->enterscope();
  expr->type_check();
  Env::object_env->exitscope();

  if (expr->get_type() == ERR_type)
  {
    set_type(ERR_type);
    return;
  }
  if (expr->get_type() == No_type)
  {
    ClassTable::semant_error(Env::cur_class) << "Expression of case statement cannot be No_type" << endl;
    set_type(ERR_type);
    return;
  }
  Symbol expr_type = expr->get_type();

  // do type-checking for cases
  std::vector<Symbol> *branch_types = new std::vector<Symbol>();
  for (int i = cases->first(); cases->more(i); i = cases->next(i))
  {
    branch_class *branch = static_cast<branch_class *>(cases->nth(i));
    if (Env::class_map->find(branch->getTypeDecl()) == Env::class_map->end())
    {
      ClassTable::semant_error(Env::cur_class);
      set_type(ERR_type);
      return;
    }

    Env::object_env->enterscope();
    Env::object_env->addid(branch->getName(), branch->getTypeDecl());
    branch->getExpr()->type_check();
    Env::object_env->exitscope();

    if (branch->getExpr()->get_type() == ERR_type)
    {
      set_type(ERR_type);
      return;
    }
    branch_types->push_back(branch->getExpr()->get_type());
  }
  
  // get LCA
  set_type(ClassTable::get_LCA(branch_types));
  delete branch_types;
}

void loop_class::type_check()
{
  // do type-checking for pred
  Env::object_env->enterscope();
  pred->type_check();
  Env::object_env->exitscope();

  if (pred->get_type() != Bool)
  {
    if (pred->get_type() != ERR_type)
      ClassTable::semant_error(Env::cur_class) << "Predicate of loop statement is not of type Bool" << endl;
    set_type(ERR_type);
    return;
  }

  // do type-checking for body
  Env::object_env->enterscope();
  body->type_check();
  Env::object_env->exitscope();
  
  if (body->get_type() == ERR_type)
  {
    set_type(ERR_type);
    return;
  }
  set_type(Object);
}

void isvoid_class::type_check()
{
  // do type-checking for e1
  Env::object_env->enterscope();
  e1->type_check();
  Env::object_env->exitscope();

  if (e1->get_type() == ERR_type)
  {
    set_type(ERR_type);
    return;
  }
  set_type(Bool);
}

void comp_class::type_check()
{
  // do type-checking for e1
  Env::object_env->enterscope();
  e1->type_check();
  Env::object_env->exitscope();

  if (e1->get_type() != Bool)
  {
    if (e1->get_type() != ERR_type)
      ClassTable::semant_error(Env::cur_class) << "Expression of comp statement is not of type Bool" << endl;
    set_type(ERR_type);
    return;
  }
  set_type(Bool);
}

void lt_class::type_check()
{
  // do type-checking for e1, e2
  Env::object_env->enterscope();
  e1->type_check();
  Env::object_env->exitscope();

  Env::object_env->enterscope();
  e2->type_check();
  Env::object_env->exitscope();

  if (e1->get_type() != Int || e2->get_type() != Int)
  {
    if (e1->get_type() != ERR_type && e2->get_type() != ERR_type)
      ClassTable::semant_error(Env::cur_class) << "Expression of lt statement is not of type Int" << endl;
    set_type(ERR_type);
    return;
  }
  set_type(Bool);
}

void leq_class::type_check()
{
  // do type-checking for e1, e2
  Env::object_env->enterscope();
  e1->type_check();
  Env::object_env->exitscope();

  Env::object_env->enterscope();
  e2->type_check();
  Env::object_env->exitscope();

  if (e1->get_type() != Int || e2->get_type() != Int)
  {
    if (e1->get_type() != ERR_type && e2->get_type() != ERR_type)
      ClassTable::semant_error(Env::cur_class) << "Expression of leq statement is not of type Int" << endl;
    set_type(ERR_type);
    return;
  }
  set_type(Bool);
}

void neg_class::type_check()
{
  // do type-checking for e1
  Env::object_env->enterscope();
  e1->type_check();
  Env::object_env->exitscope();

  if (e1->get_type() != Int)
  {
    if (e1->get_type() != ERR_type)
      ClassTable::semant_error(Env::cur_class) << "Expression of neg statement is not of type Int" << endl;
    set_type(ERR_type);
    return;
  }
  set_type(Int);
}

void mul_class::type_check()
{
  // do type-checking for e1, e2
  Env::object_env->enterscope();
  e1->type_check();
  Env::object_env->exitscope();

  Env::object_env->enterscope();
  e2->type_check();
  Env::object_env->exitscope();

  if (e1->get_type() != Int || e2->get_type() != Int)
  {
    if (e1->get_type() != ERR_type && e2->get_type() != ERR_type)
      ClassTable::semant_error(Env::cur_class) << "Expression of mul statement is not of type Int" << endl;
    set_type(ERR_type);
    return;
  }
  set_type(Int);
}

void plus_class::type_check()
{
  // do type-checking for e1, e2
  Env::object_env->enterscope();
  e1->type_check();
  Env::object_env->exitscope();

  Env::object_env->enterscope();
  e2->type_check();
  Env::object_env->exitscope();

  if (e1->get_type() != Int || e2->get_type() != Int)
  {
    if (e1->get_type() != ERR_type && e2->get_type() != ERR_type)
      ClassTable::semant_error(Env::cur_class) << "Expression of plus statement is not of type Int" << endl;
    set_type(ERR_type);
    return;
  }
  set_type(Int);
}

void sub_class::type_check()
{
  // do type-checking for e1, e2
  Env::object_env->enterscope();
  e1->type_check();
  Env::object_env->exitscope();

  Env::object_env->enterscope();
  e2->type_check();
  Env::object_env->exitscope();

  if (e1->get_type() != Int || e2->get_type() != Int)
  {
    if (e1->get_type() != ERR_type && e2->get_type() != ERR_type)
      ClassTable::semant_error(Env::cur_class) << "Expression of sub statement is not of type Int" << endl;
    set_type(ERR_type);
    return;
  }
  set_type(Int);
}

void divide_class::type_check()
{
  // do type-checking for e1, e2
  Env::object_env->enterscope();
  e1->type_check();
  Env::object_env->exitscope();

  Env::object_env->enterscope();
  e2->type_check();
  Env::object_env->exitscope();

  if (e1->get_type() != Int || e2->get_type() != Int)
  {
    if (e1->get_type() != ERR_type && e2->get_type() != ERR_type)
      ClassTable::semant_error(Env::cur_class) << "Expression of divide statement is not of type Int" << endl;
    set_type(ERR_type);
    return;
  }
  set_type(Int);
}

void eq_class::type_check() {
  // do type-checking for e1, e2
  Env::object_env->enterscope();
  e1->type_check();
  Env::object_env->exitscope();
  Symbol t = e1->get_type();
  if (t != Int || t != Str || t != Bool)
  {
    ClassTable::semant_error(Env::cur_class) << "Expression of eq statement is not of type Int, Str or Bool";
    set_type(ERR_type);
    return;
  }

  Env::object_env->enterscope();
  e2->type_check();
  Env::object_env->exitscope();
  t = e2->get_type();
  if (t != Int || t != Str || t != Bool)
  {
    ClassTable::semant_error(Env::cur_class) << "Expression of eq statement is not of type Int, Str or Bool";
    set_type(ERR_type);
    return;
  }

  if (e1->get_type() != e2->get_type())
  {
    if (e1->get_type() != ERR_type && e2->get_type() != ERR_type)
      ClassTable::semant_error(Env::cur_class) << "Expressions of eq statement have different types";
    set_type(ERR_type);
    return;
  }
  set_type(Bool);
}

void no_expr_class::type_check() { set_type(No_type); }

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
  if (classtable->errors())
  {
    cerr << "Compilation halted due to static semantic errors." << endl;
    exit(1);
  }

  /* some semantic analysis code may go here */
  // Here, we conduct Type checking.
  type_check();

  if (classtable->errors())
  {
    cerr << "Compilation halted due to static semantic errors." << endl;
    exit(1);
  }
  
  std::cout << "All semanteme have past checking!\nREADY TO GO!\n\nSyntax tree:\n";
}