//
//  libgraciela-abstract.cpp
//  libgraciela-abstract
//
//  Created by Carlos Spaggiari Roa on 8/24/16.
//  Copyright © 2016 ARSC. All rights reserved.
//

#include "libgraciela-abstract.h"

#ifdef __cplusplus


extern "C" {

  using namespace glib;

  vector<TrashCollector>* _stack;
  set<int8_t*> dynMemSet;

//  int precondition = 0;

  void abortAbstract(abortEnum reason, char* filePath, int line, int col, int pos = 0, int size = 0){

    printf ("\x1B[0;31mABORT:\x1B[m %s at line %d, column %d:\n\t", filePath, line, col);

    switch (reason) {
      case A_DUPLICATE_DOMAIN:
        printf ("Duplicate value in domain.\n");
        break;
      case A_NOT_IN_DOMAIN:
        printf ("Value not in domain\n");
        break;
      case A_NEGATIVE_POS:
        printf ("Trying to access negative position `%d` in a sequence.\n", pos);
        break;
      case A_BAD_POS:
        printf ("Trying to access the position `%d` of a sequence of size `%d`.\n", pos, size);
        break;
      case A_REMOVE_POINTER:
        printf ("Trying to free a pointer that was already freed\n\t");
        printf ("or not allocated with `new` instruction.\n");
        break;
      case A_NULL_POINTER:
        printf ("A null pointer was dereferenced.\n");
        break;
      case A_BAD_POINTER:
        printf ("Attempted to dereference a bad pointer. Maybe it was already freed.\n");
        break;
      default:
        break;
    }

    _freeTrashCollector();
    exit(EXIT_FAILURE);
  }

  void mark(TCTuple t){
    (*_stack)[_stack->size()-1].push_back(t);
  }

  /* Set */
  int8_t *_newSet(){
      Set *set = new Set;
      mark(TCTuple((int8_t*)set,SET));
      return (int8_t*)set;
  }

  Iterator *_firstSet(int8_t *ptr){
      Set* set = (Set*)ptr;

      Set::iterator *it =(Set::iterator*)malloc (sizeof(Set::iterator));
      *it = set->begin();
      t data = **it;
      if (set->begin() == set->end()){
          free(it);
          return NULL;
      }
      else {
          Iterator *i = (Iterator*)malloc(sizeof(Iterator));
          mark(TCTuple((int8_t*) i, ITERATOR));
          i->data = data;
          i->it = (int8_t*) it;
          i->type = ptr;
          return i;
      }
  }
  Iterator *_nextSet(Iterator* i){
      Set::iterator *next = (Set::iterator*)i->it;
      Set* type = (Set*)i->type;

      *next = ++(*next);
      if (*next != type->end()) {
          Iterator *_i = (Iterator*)malloc(sizeof(Iterator));
          mark(TCTuple((int8_t*) _i, ITERATOR));
          _i->data = **next;
          _i->it = (int8_t*) next;
          _i->type = i->type;
          return _i;
      } else {
          free(i->it);
          return NULL;
      }
  }

  Iterator *_firstMultiset(int8_t *ptr){
    Multiset* set = (Multiset*)ptr;

    Multiset::iterator *it =(Multiset::iterator*)malloc (sizeof(Multiset::iterator));
    *it = set->begin();
    t data = **it;
    if (set->begin() == set->end()){
      free(it);
      return NULL;
    }
    else {
      Iterator *i = (Iterator*)malloc(sizeof(Iterator));
      mark(TCTuple((int8_t*) i, ITERATOR));
      i->data = data;
      i->it = (int8_t*) it;
      i->type = ptr;
      return i;
    }
  }
  Iterator *_nextMultiset(Iterator* i){
    Multiset::iterator *next = (Multiset::iterator*)i->it;
    Multiset* type = (Multiset*)i->type;

    *next = ++(*next);
    if (*next != type->end()) {
      Iterator *_i = (Iterator*)malloc(sizeof(Iterator));
      mark(TCTuple((int8_t*) _i, ITERATOR));
      _i->data = **next;
      _i->it = (int8_t*) next;
      _i->type = i->type;
      return _i;
    } else {
      free(i->it);
      return NULL;
    }
  }

  Iterator *_firstSequence(int8_t *ptr){
    Sequence* set = (Sequence*)ptr;

    Sequence::iterator *it =(Sequence::iterator*)malloc (sizeof(Sequence::iterator));
    *it = set->begin();
    t data = **it;
    if (set->begin() == set->end()){
      free(it);
      return NULL;
    }
    else {
      Iterator *i = (Iterator*)malloc(sizeof(Iterator));
      mark(TCTuple((int8_t*) i, ITERATOR));
      i->data = data;
      i->it = (int8_t*) it;
      i->type = ptr;
      return i;
    }
  }

  Iterator *_nextSequence(Iterator* i){
    Sequence::iterator *next = (Sequence::iterator*)i->it;
    Sequence* type = (Sequence*)i->type;

    *next = ++(*next);
    if (*next != type->end()) {
      Iterator *_i = (Iterator*)malloc(sizeof(Iterator));
      mark(TCTuple((int8_t*) _i, ITERATOR));
      _i->data = **next;
      _i->it = (int8_t*) next;
      _i->type = i->type;
      return _i;
    } else {
      free(i->it);
      return NULL;
    }
  }

  int _equalSet(int8_t *ptr1, int8_t* ptr2){
      return *(Set*)ptr1 == *(Set*)ptr2;
  }

  void _insertSet(int8_t *ptr, t x){
      Set *s = (Set*) ptr;
      s->insert(x);
  }

  int _sizeSet(int8_t *ptr) {
      return (int) ((Set*) ptr)->size();
  }

  int _isElemSet(int8_t *ptr, t x){
      Set *s = (Set*) ptr;
      return s->find(x) != s->end();
  }

  int8_t *_unionSet(int8_t *ptr1, int8_t * ptr2) {
      Set *set1   = (Set*)ptr1,
          *set2   = (Set*)ptr2,
          *newset = (Set*)_newSet();

      set_union(set1->begin(), set1->end(),
                set2->begin(), set2->end(),
                inserter(*newset, newset->begin()));
      return (int8_t*)newset;
  }

  int8_t *_intersectSet(int8_t *ptr1, int8_t * ptr2) {
      Set *set1   = (Set*)ptr1,
          *set2   = (Set*)ptr2,
          *newset = (Set*)_newSet();

      set_intersection(set1->begin(), set1->end(),
                       set2->begin(), set2->end(),
                       inserter(*newset, newset->begin()));

      return (int8_t*)newset;
  }

  int8_t *_differenceSet(int8_t *ptr1, int8_t * ptr2) {
      Set *set1   = (Set*)ptr1,
          *set2   = (Set*)ptr2,
          *newset = (Set*)_newSet();

      set_difference(set1->begin(), set1->end(),
                     set2->begin(), set2->end(),
                     inserter(*newset, newset->begin()));

      return (int8_t*)newset;
  }

  int _includesSet(int8_t* ptr1, int8_t* ptr2){
    Set *set1 = (Set*)ptr1,
        *set2 = (Set*)ptr2;

    return includes(set1->begin(), set1->end(),
                    set2->begin(), set2->end());
  }

  int _includesSSet(int8_t* ptr1, int8_t* ptr2){
    Set *set1 = (Set*)ptr1,
        *set2 = (Set*)ptr2;

    // a ⊃ b  ≡ a ⊇ b ∧ |a| > |b|
    return includes(set1->begin(), set1->end(),
                    set2->begin(), set2->end())
           and set1->size() > set2->size();
  }

  void _freeSet(int8_t* ptr) {
      delete (Set*)ptr;
  }

  /* SetPair */
  int8_t *_newSetPair(){
    SetPair *set = new SetPair;
    mark(TCTuple((int8_t*)set,SETPAIR));
    return (int8_t*)set;
  }

  int _equalSetPair(int8_t *ptr1, int8_t* ptr2){
    return *(SetPair*)ptr1 == *(SetPair*)ptr2;
  }

  void _insertSetPair(int8_t *ptr, gtuple *x){
    SetPair *s = (SetPair*) ptr;
    s->insert(Tuple(x->a,x->b));
  }

//  int _sizeSetPair(int8_t *ptr) {
//    return (int) ((Set*) ptr)->size();
//  }
//
  int _isElemSetPair(int8_t *ptr, gtuple *x){
    SetPair *s = (SetPair*) ptr;
    return s->find(Tuple(x->a,x->b)) != s->end();
  }

  int8_t* _unionSetPair(int8_t *ptr1, int8_t * ptr2) {
    SetPair *set1   = (SetPair*)ptr1,
            *set2   = (SetPair*)ptr2,
            *newset = (SetPair*)_newSet();

    set_union(set1->begin(), set1->end(),
              set2->begin(), set2->end(),
              inserter(*newset, newset->begin()));

    return (int8_t*)newset;
  }

  int8_t* _intersectSetPair(int8_t *ptr1, int8_t * ptr2) {
    SetPair *set1   = (SetPair*)ptr1,
            *set2   = (SetPair*)ptr2,
            *newset = (SetPair*)_newSet();

    set_intersection(set1->begin(), set1->end(),
                     set2->begin(), set2->end(),
                     inserter(*newset, newset->begin()));

    return (int8_t*)newset;
  }

  int8_t* _differenceSetPair(int8_t *ptr1, int8_t * ptr2) {
    SetPair *set1   = (SetPair*)ptr1,
    *set2   = (SetPair*)ptr2,
    *newset = (SetPair*)_newSet();

    set_difference(set1->begin(), set1->end(),
                   set2->begin(), set2->end(),
                   inserter(*newset, newset->begin()));

    return (int8_t*)newset;
  }

  int _includesSetPair(int8_t* ptr1, int8_t* ptr2){
    Set *set1 = (Set*)ptr1,
    *set2 = (Set*)ptr2;

    return includes(set1->begin(), set1->end(),
                    set2->begin(), set2->end());
  }

  int _includesSSetPair(int8_t* ptr1, int8_t* ptr2){
    SetPair *set1 = (SetPair*)ptr1,
            *set2 = (SetPair*)ptr2;

    // a ⊃ b  ≡ a ⊇ b ∧ |a| > |b|
    return includes(set1->begin(), set1->end(),
                    set2->begin(), set2->end())
           and set1->size() > set2->size();
  }

  void _freeSetPair(int8_t* ptr) {
    delete (SetPair*)ptr;
  }

  /* MultiSet */

  int8_t* _newMultiset(){
      Multiset *mul = new Multiset;
      mark(TCTuple((int8_t*)mul,MULTISET));
      return (int8_t*)mul;
  }

  int _equalMultiset(int8_t *ptr1, int8_t* ptr2){
      return *(Multiset*)ptr1 == *(Multiset*)ptr2;

  }

  void _insertMultiset(int8_t *ptr, t x){
      Multiset *s = (Multiset*) ptr;
      s->insert(x);
  }

  int _isElemMultiset(int8_t *ptr, t x){
      Multiset *s = (Multiset*) ptr;
      return s->find(x) != s->end();
  }

  int _sizeMultiset(int8_t *ptr){
      return (int) ((Multiset*) ptr)->size();
  }

  int _countMultiset(t x, int8_t *ptr){
      Multiset *s = (Multiset*) ptr;
      return (int) s->count(x);
  }

  int8_t* _unionMultiset(int8_t *ptr1, int8_t * ptr2) {
      Multiset *set1   = (Multiset*)ptr1,
               *set2   = (Multiset*)ptr2,
               *newSet = (Multiset*)_newMultiset();

      set_union(set1->begin(), set1->end(),
                set2->begin(), set2->end(),
                inserter(*newSet, newSet->begin()));

      return (int8_t*)newSet;
  }

  int8_t* _sumMultiset(int8_t* ptr1, int8_t* ptr2){
    Multiset *set1   = (Multiset*)ptr1,
             *set2   = (Multiset*)ptr2,
             *newSet = (Multiset*)_newMultiset();

    for (Multiset::iterator it = set1->begin() ; it != set1->end(); ++it){
      newSet->insert(*it);
    }

    for (Multiset::iterator it = set2->begin() ; it != set2->end(); ++it){
      newSet->insert(*it);
    }

    return (int8_t*)newSet;
  }
  int8_t* _intersectMultiset(int8_t *ptr1, int8_t * ptr2) {
      Multiset *set1   = (Multiset*)ptr1,
               *set2   = (Multiset*)ptr2,
               *newSet = (Multiset*)_newMultiset();

      set_intersection(set1->begin(), set1->end(),
                       set2->begin(), set2->end(),
                       inserter(*newSet, newSet->begin()));

      return (int8_t*)newSet;
  }

  int8_t* _differenceMultiset(int8_t *ptr1, int8_t * ptr2) {
      Multiset *set1   = (Multiset*)ptr1,
               *set2   = (Multiset*)ptr2,
               *newSet = (Multiset*)_newMultiset();

      set_difference(set1->begin(), set1->end(),
                     set2->begin(), set2->end(),
                     inserter(*newSet, newSet->begin()));

      return (int8_t*)newSet;
  }

  int _includesMultiset(int8_t* ptr1, int8_t* ptr2){
      Multiset *set1 = (Multiset*)ptr1,
               *set2 = (Multiset*)ptr2;

      return includes(set1->begin(), set1->end(),
                      set2->begin(), set2->end());
  }

  int _includesSMultiset(int8_t* ptr1, int8_t* ptr2){
    Multiset *set1 = (Multiset*)ptr1,
             *set2 = (Multiset*)ptr2;

    return includes(set1->begin(), set1->end(),
                    set2->begin(), set2->end())
           and set1->size() > set2->size();
  }

  void _freeMultiset(int8_t* ptr) {
      delete (Multiset*)ptr;
  }

  /* MultiSetPair */

  int8_t* _newMultisetPair(){
    MultisetPair *mul = new MultisetPair;
    mark(TCTuple((int8_t*)mul,MULTISETPAIR));
    return (int8_t*)mul;
  }

  int _equalMultisetPair(int8_t *ptr1, int8_t* ptr2){
    return *(MultisetPair*)ptr1 == *(MultisetPair*)ptr2;

  }

  void _insertMultisetPair(int8_t *ptr, gtuple* x){
    MultisetPair *s = (MultisetPair*) ptr;
    s->insert(Tuple(x->a,x->b));
  }

  int _isElemMultisetPair(int8_t *ptr, gtuple* x){
    MultisetPair *s = (MultisetPair*) ptr;
    return s->find(Tuple(x->a,x->b)) != s->end();
  }

//  int _sizeMultisetPair(int8_t *ptr){
//    return (int) ((MultisetPair*) ptr)->size();
//  }

  int _countMultisetPair(gtuple* x, int8_t *ptr){
    MultisetPair *s = (MultisetPair*) ptr;
    return (int) s->count(Tuple(x->a,x->b));
  }

  int8_t* _unionMultisetPair(int8_t *ptr1, int8_t * ptr2) {
    MultisetPair *set1   = (MultisetPair*)ptr1,
                 *set2   = (MultisetPair*)ptr2,
                 *newSet = (MultisetPair*)_newMultisetPair();

    set_union(set1->begin(), set1->end(),
              set2->begin(), set2->end(),
              inserter(*newSet, newSet->begin()));

    return (int8_t*)newSet;
  }

  int8_t* _sumMultisetPair(int8_t* ptr1, int8_t* ptr2){
    MultisetPair *set1   = (MultisetPair*)ptr1,
                 *set2   = (MultisetPair*)ptr2,
                 *newSet = (MultisetPair*)_newMultisetPair();

    for (MultisetPair::iterator it = set1->begin() ; it != set1->end(); ++it){
      newSet->insert(*it);
    }

    for (MultisetPair::iterator it = set2->begin() ; it != set2->end(); ++it){
      newSet->insert(*it);
    }

    return (int8_t*)newSet;
  }
  int8_t* _intersectMultisetPair(int8_t *ptr1, int8_t * ptr2) {
    MultisetPair *set1   = (MultisetPair*)ptr1,
                 *set2   = (MultisetPair*)ptr2,
                 *newSet = (MultisetPair*)_newMultisetPair();

    set_intersection(set1->begin(), set1->end(),
                     set2->begin(), set2->end(),
                     inserter(*newSet, newSet->begin()));

    return (int8_t*)newSet;
  }

  int8_t* _differenceMultisetPair(int8_t *ptr1, int8_t * ptr2) {
    MultisetPair *set1   = (MultisetPair*)ptr1,
                 *set2   = (MultisetPair*)ptr2,
                 *newSet = (MultisetPair*)_newMultisetPair();

    set_difference(set1->begin(), set1->end(),
                   set2->begin(), set2->end(),
                   inserter(*newSet, newSet->begin()));

    return (int8_t*)newSet;
  }

  int _includesMultisetPair(int8_t* ptr1, int8_t* ptr2){
    MultisetPair *set1 = (MultisetPair*)ptr1,
                 *set2 = (MultisetPair*)ptr2;

    return includes(set1->begin(), set1->end(),
                    set2->begin(), set2->end());
  }

  int _includesSMultisetPair(int8_t* ptr1, int8_t* ptr2){
    MultisetPair *set1 = (MultisetPair*)ptr1,
                 *set2 = (MultisetPair*)ptr2;

    return includes(set1->begin(), set1->end(),
                    set2->begin(), set2->end())
    and set1->size() > set2->size();
  }

  void _freeMultisetPair(int8_t* ptr) {
    delete (MultisetPair*)ptr;
  }

  /* Function */

  int8_t *_newFunction(){
      Function *function = new Function();
      mark(TCTuple((int8_t*)function,FUNCTION));
      return (int8_t*) function;
  }

  int _equalFunction(int8_t* ptr1, int8_t* ptr2){
      return *(Function*)ptr1 == *(Function*)ptr2;
  }

  void _insertFunction(int8_t *ptr, t key, t value) {
      ((Function*)ptr)->insert(Tuple(key,value));
  }

  int8_t *_funcFromSet(int8_t* setPtr, char* filePath, int line, int col){
      SetPair* set   = (SetPair*)setPtr;
      Function *func = (Function*)_newFunction();

    for(SetPair::iterator it = set->begin(); it != set->end(); ++it){
        Function::iterator it2 = func->find(it->first);
        if (it2 != func->end()) {
          abortAbstract(A_DUPLICATE_DOMAIN, filePath, line, col);
        } else {
          func->insert(Tuple(it->first,it->second));
        }
      }
      return (int8_t*)func;
  }

  int _sizeFunction(int8_t *ptr){
      return (int) ((Function*)ptr)->size();
  }

  int _isElemFunction(int8_t *ptr, t key, t value) {
      Function *function = (Function*)ptr;
      Function::iterator it = function->find(key);

      if (function->find(key) != function->end())
      {
          return (it->second == value);
      }

      return false;
  }

  int8_t *_domFunction(int8_t *ptr){
      Function *function = (Function*)ptr;
      Set      *set = (Set*)_newSet();
      for(Function::iterator it = function->begin(); it != function->end(); ++it){
          set->insert(it->first);
      }
      
      return (int8_t*) set;
  }

  int8_t *_codomainFunction(int8_t *ptr){
    Function *function = (Function*)ptr;
    Set      *set = (Set*)_newSet();
    for(Function::iterator it = function->begin(); it != function->end(); ++it){
      set->insert(it->second);
    }

    return (int8_t*)set;
  }

  t _pairFunction(int8_t *ptr, t k, char* filePath, int line, int col){
    Function *function    = (Function*)ptr;
    Function::iterator it = function->find(k);
    if (it == function->end()){
      abortAbstract(A_NOT_IN_DOMAIN, filePath, line, col);
    }
    return (t)function->find(k)->second;

  }

  int8_t *_compositionFunction(int8_t *ptr1, int8_t *ptr2){
      Function *f1      = (Function*)ptr1,
               *f2      = (Function*)ptr2,
               *newFunc = (Function*)_newFunction();

      for(Function::iterator it = f1->begin(); it != f1->end(); ++it){

          Function::iterator it2 = f2->find(it->second);
          if ( it2 != f2->end())
              newFunc->insert(Tuple(it->first,it2->second));
      }

      return (int8_t*)newFunc;
  }

  int8_t* _toSetFunction(int8_t* ptr1){
    Function* func = (Function*) ptr1;
    SetPair* set = (SetPair*)_newSetPair();

    for(Function::iterator it = func->begin(); it != func->end(); ++it){
      set->insert(Tuple(it->first, it->second));
    }
    return (int8_t*) set;
  }

  int8_t* _unionFunction(int8_t* ptr1, int8_t* ptr2, char* filePath, int line, int col){
    Function *func1    = (Function*)ptr1,
             *func2    = (Function*)ptr2,
             *newFunc = (Function*)_newMultisetPair();

    for(Function::iterator it = func1->begin(); it != func1->end(); ++it){
      newFunc->insert(Tuple(it->first, it->second));
    }

    for(Function::iterator it = func2->begin(); it != func2->end(); ++it){
      Function::iterator it2 = newFunc->find(it->first);
      if (it2 != newFunc->end()){
        if (it2->second != it->second){
          abortAbstract(A_DUPLICATE_DOMAIN, filePath, line, col);
        }
      } else {
        newFunc->insert(Tuple(it->first, it->second));
      }

    }

    return (int8_t*)newFunc;
  }




  int8_t* _intersectFunction(int8_t* ptr1, int8_t* ptr2){
    Relation *func1   = (Relation*)ptr1,
             *func2   = (Relation*)ptr2,
             *newfunc = (Relation*)_newRelation();

    set_intersection(func1->begin(), func1->end(),
                     func2->begin(), func2->end(),
                     inserter(*newfunc, newfunc->begin()));

    return (int8_t*)newfunc;
  }

  int8_t* _differenceFunction(int8_t* ptr1, int8_t* ptr2){
    Relation *func1   = (Relation*)ptr1,
             *func2   = (Relation*)ptr2,
             *newfunc = (Relation*)_newRelation();

    set_difference(func1->begin(), func1->end(),
                   func2->begin(), func2->end(),
                   inserter(*newfunc, newfunc->begin()));

    return (int8_t*)newfunc;
  }

  void _freeFunction(int8_t* ptr) {
      delete (Function*)ptr;
  }

  /* Relation */


  int8_t *_newRelation(){
      Relation *rel = new Relation();
      mark(TCTuple((int8_t*)rel,RELATION));
      return (int8_t*) rel;
  }

  int _equalRelation(int8_t *ptr1, int8_t *ptr2){
      return *(Relation*)ptr1 == *(Relation*)ptr2;
  }

  void _insertRelation(int8_t *ptr, t key, t value) {
      ((Relation*)ptr)->insert(Tuple(key,value));
  }

  int8_t *_relationFromSet(int8_t* setPtr){
      SetPair* set  = (SetPair*)setPtr;
      Relation* rel = (Relation*)_newRelation();
      *rel = *set;
      return (int8_t*)rel;
  }

  int _sizeRelation(int8_t *ptr){
      return (int) ((Relation*)ptr)->size();
  }

  int _isElemRelation(int8_t *ptr, t key, t value) {
      Relation *rel = (Relation*)ptr;
      return rel->find(Tuple(key,value)) != rel->end();
  }

  int8_t* _domRelation(int8_t *ptr){
      Relation *rel = (Relation*)ptr;
      Set      *set = (Set*)_newSet();
      for(Relation::iterator it = rel->begin(); it != rel->end(); ++it){
          set->insert(it->first);
      }
      return (int8_t*) set;
  }

  int8_t* _codomainRelation(int8_t *ptr){
    Relation *rel = (Relation*)ptr;
    Set      *set = (Set*)_newSet();
    for(Relation::iterator it = rel->begin(); it != rel->end(); ++it){
      set->insert(it->second);
    }
    return (int8_t*) set;
  }

  int8_t* _pairRelation(int8_t *ptr, t key){
      Relation *rel = (Relation*)ptr;
      Set      *set = (Set*)_newSet();

      for(Relation::iterator it = rel->begin(); it != rel->end(); ++it){
          if (it->first == key)
              set->insert(it->second);
      }
      return (int8_t*) set;
  }

  int8_t *_compositionRelation(int8_t *ptr1, int8_t *ptr2){
      Relation *r1     = (Relation*)ptr1,
               *newRel = (Relation*)_newRelation();


      for(Relation::iterator it = r1->begin(); it != r1->end(); ++it){

          Set *rge2 = (Set*)_pairRelation(ptr2, it->second);
          for(Set::iterator it2 = rge2->begin(); it2 != rge2->end(); ++it2){
              newRel->insert(Tuple(it->first,*it2));
          }
      }

      return (int8_t*)newRel;
  }

  int8_t* _toSetRelation(int8_t* ptr1){
    return ptr1;
  }

  void _freeRelation(int8_t* ptr) {
      delete (Relation*)ptr;
  }

  /* Sequence */

  int8_t *_newSequence(){
      Sequence *s = new Sequence;
      mark(TCTuple((int8_t*)s,SEQUENCE));
      return (int8_t*) s;
  }

  int _equalSequence(int8_t* ptr1, int8_t* ptr2){
      return *(Sequence*)ptr1 == *(Sequence*)ptr2;
  }

  int _isElemSequence(int8_t *ptr, t x){
      Sequence *seq = (Sequence*)ptr;
      for (Sequence::iterator it = seq->begin() ; it != seq->end() ; ++it){
          if (*it == x) return true;
      }
      return false;
  }
  int _countSequence(t x, int8_t* ptr){
    Sequence* seq = (Sequence*)ptr;
    int i = 0;
    for (Sequence::iterator it = seq->begin(); it != seq->end(); ++it) {
      if (*it == x) {
        i++;
      }
    }
    return i;
  }
  void _insertSequence(int8_t* ptr, t x){
      ((Sequence*)ptr)->push_back(x);
  }

  int8_t* _concatSequence(int8_t* ptr1, int8_t* ptr2){
    Sequence *seq1   = (Sequence*)ptr1,
             *seq2   = (Sequence*)ptr2,
             *newSeq = (Sequence*)_newSequence();
    newSeq->insert( newSeq->end(), seq1->begin(), seq1->end());
    newSeq->insert( newSeq->end(), seq2->begin(), seq2->end());
    return (int8_t*)newSeq;
  }

  int _sizeSequence(int8_t* ptr){
      return (int) ((Sequence*)ptr)->size();
  }

  t _atSequence(int8_t*ptr, int pos, char* filePath, int line, int col){
    Sequence* seq = (Sequence*)ptr;
    if (pos < 0){
      abortAbstract(A_NEGATIVE_POS, filePath, line, col);
    }
    else if (pos >= seq->size()){
      abortAbstract(A_BAD_POS, filePath, line, col);
    }
    return (*seq)[pos];
  }

  void _freeSequence(int8_t* ptr) {
      delete (Sequence*)ptr;
  }

  int _equalTuple(gtuple* x, gtuple* y){
    return x->a == y->a and x->b == y->b;
  }
  /* SequencePair */

  int8_t *_newSequencePair(){
    SequencePair *s = new SequencePair;
    mark(TCTuple((int8_t*)s,SEQUENCEPAIR));
    return (int8_t*) s;
  }

  int _equalSequencePair(int8_t* ptr1, int8_t* ptr2){
    return *(SequencePair*)ptr1 == *(SequencePair*)ptr2;
  }

  int _isElemSequencePair(int8_t *ptr, gtuple* x){
    SequencePair *seq = (SequencePair*)ptr;
    for (SequencePair::iterator it = seq->begin() ; it != seq->end() ; ++it){
      if (*it == Tuple(x->a,x->b)) return true;
    }
    return false;
  }
  int _countSequencePair(gtuple* x, int8_t* ptr){
    SequencePair* seq = (SequencePair*)ptr;
    int i = 0;
    for (SequencePair::iterator it = seq->begin(); it != seq->end(); ++it) {
      if (*it == Tuple(x->a,x->b)) {
        i++;
      }
    }
    return i;
  }
  void _insertSequencePair(int8_t* ptr, gtuple* x){
    ((SequencePair*)ptr)->push_back(Tuple(x->a,x->b));
  }

  int8_t* _concatSequencePair(int8_t* ptr1, int8_t* ptr2){
    SequencePair *seq1   = (SequencePair*)ptr1,
                 *seq2   = (SequencePair*)ptr2,
                 *newSeq = (SequencePair*)_newSequencePair();
    newSeq->insert( newSeq->end(), seq1->begin(), seq1->end());
    newSeq->insert( newSeq->end(), seq2->begin(), seq2->end());
    return (int8_t*)newSeq;
  }

//  int _sizeSequencePair(int8_t* ptr){
//    return (int) ((SequencePair*)ptr)->size();
//  }

  gtuple _atSequencePair(int8_t*ptr, int pos, char* filePath, int line, int col){
    SequencePair* seq = (SequencePair*)ptr;
    if (pos < 0){
      abortAbstract(A_NEGATIVE_POS, filePath, line, col,pos);
    }
    else if (pos >= seq->size()){
      abortAbstract(A_BAD_POS, filePath, line, col,pos,(int)seq->size());
    }
    Tuple t = (*seq)[pos];
    gtuple tuple = {t.first, t.second};
    return tuple;
  }

  void _freeSequencePair(int8_t* ptr) {
    delete (SequencePair*)ptr;
  }


  /* Trash Collector */

  void _initTrashCollector(){
    srand (time(NULL));
    _stack = new vector<TrashCollector>;
  }

  void _openScope(){
      TrashCollector tc = TrashCollector();
      tc.reserve(64);
      _stack->push_back(tc);
  }

  void _closeScope(){
      TrashCollector tc = (*_stack)[_stack->size()-1];
      _stack->pop_back();
      for (TrashCollector::iterator it = tc.begin(); it != tc.end(); ++it){
          // printf("Free MEM -> %p %d \n",  it->first, it->second);
          switch (it->second) {
              case SET: {
                  _freeSet(it->first);
                  break;
              }
              case MULTISET: {
                  _freeMultiset(it->first);
                  break;
              }
              case FUNCTION: {
                  _freeFunction(it->first);
                  break;
              }
              case RELATION: {
                  _freeRelation(it->first);
                  break;
              }
              case SEQUENCE: {
                  _freeSequence(it->first);
                  break;
              }
              case SETPAIR: {
                  _freeSetPair(it->first);
                  break;
              }
              case MULTISETPAIR: {
                _freeMultisetPair(it->first);
                break;
              }
              case SEQUENCEPAIR: {
                _freeSequencePair(it->first);
                break;
              }
              case ITERATOR: {
                free((Iterator*)it->first);
                break;
              }
              case MEM: {
                free(it->first);
                break;
            }
          }
      }
      tc.clear();
  }

  void _freeTrashCollector(){
      for (int i = 0 ; i < _stack->size(); i++)
      {
          _closeScope();
      }
      for (set<int8_t*>::iterator it = dynMemSet.begin(); it != dynMemSet.end(); ++it){
        // printf("Free Leaks -> %p\n", *it);
        free(*it);
      }
      dynMemSet.clear();
      delete (_stack);
  }
}


/* Dynamic Memory Verifications */
void _addPointer(int8_t* ptr){
  dynMemSet.insert(ptr);
  // printf("Add -> %p\n", ptr);
}

void _removePointer(int8_t* ptr, char* filePath, int l, int c){
  set<int8_t*>::iterator p = dynMemSet.find(ptr);
  // printf("Remove -> %p\n", ptr);
  if ( p == dynMemSet.end()) {
    abortAbstract(A_REMOVE_POINTER, filePath, l, c);
    exit(EXIT_FAILURE);
  } else {
    dynMemSet.erase(ptr);
  }
}

void _derefPointer(int8_t* ptr, int p, char* filePath, int l, int c){
  if (ptr == 0){
    abortAbstract(A_NULL_POINTER, filePath, l, c);
    exit(EXIT_FAILURE);
  }
  else if (not p && dynMemSet.find(ptr) == dynMemSet.end()) {
    abortAbstract(A_BAD_POINTER, filePath, l, c);
    exit(EXIT_FAILURE);
  }
}

char *_readln(int *i){
  std::string str;
  std::getline(std::cin, str);
  *i = str.size();
  char *cstr = (char*) malloc(str.length() + 1);
  strcpy(cstr, str.c_str());
  _addPointer((int8_t*)cstr); 
  return cstr;
}

#endif
