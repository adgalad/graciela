//
//  libgraciela-abstract.h
//  libgraciela-abstract
//
//  Created by Carlos Spaggiari Roa on 8/20/16.
//  Copyright © 2016 ARSC. All rights reserved.
//

#ifndef Header_h
#define Header_h

#ifdef __cplusplus
#include <cstdio>
#include <cstdlib>
#include <iostream>
#include <set>
#include <map>
#include <vector>
#include <algorithm>
#include <time.h>
#else
#include <stdlib.h>
#endif


typedef int64_t t;
#ifdef __cplusplus
using namespace std;
namespace glib {

    typedef enum{
      MEM           = 0,
      SET           = 1,
      MULTISET      = 2,
      FUNCTION      = 3,
      RELATION      = 4,
      SEQUENCE      = 5,
      SETPAIR       = 6,
      MULTISETPAIR  = 7,
      SEQUENCEPAIR  = 8,
      ITERATOR      = 9
    } type;

    typedef map<t,t>           Function;
    typedef pair<t,t>          Tuple;
    typedef set<t>             Set;
    typedef set<Tuple>         SetPair;
    typedef set<Tuple>         Relation;
    typedef multiset<t>        Multiset;
    typedef multiset<Tuple>    MultisetPair;
    typedef vector<t>          Sequence;
    typedef vector<Tuple>      SequencePair;
    typedef pair<int8_t*,type> TCTuple;
    typedef vector<TCTuple>    TrashCollector;

  typedef enum{
    A_DUPLICATE_DOMAIN = 0,
    A_NOT_IN_DOMAIN,
    A_NEGATIVE_POS,
    A_BAD_POS,
    A_REMOVE_POINTER,
    A_NULL_POINTER,
    A_BAD_POINTER,
  } abortEnum;
}

extern "C" {
#endif

    typedef struct gtuple {
        int64_t a;
        int64_t b;
    } gtuple;

    typedef struct Iterator {
        t       data;
        int8_t* it;
        int8_t* type;
    }Iterator;

    Iterator *___firstSet(int8_t *ptr);
    Iterator *___nextSet(Iterator* i);

    Iterator *___firstMuliset(int8_t *ptr);
    Iterator *___nextMultiset(Iterator* i);

    Iterator *___firstSequence(int8_t *ptr);
    Iterator *___nextSequence(Iterator* i);



    /* Set */
    int8_t*  ___newSet();
    int      ___equalSet(int8_t* ptr1, int8_t* ptr2);
    void     ___insertSet(int8_t* ptr, t x);
    int      ___sizeSet(int8_t* ptr);
    int      ___isElemSet(int8_t* ptr, t x);
    int8_t*  ___unionSet(int8_t* ptr1, int8_t* ptr2);
    int8_t*  ___intersectSet(int8_t* ptr1, int8_t* ptr2);
    int8_t*  ___differenceSet(int8_t* ptr1, int8_t* ptr2);
    int      ___includesSet(int8_t* ptr1, int8_t* ptr2);
    int      ___includesSSet(int8_t* ptr1, int8_t* ptr2);
    void     ___freeSet(int8_t* ptr);
    /* SetPair */

    int8_t* ___newSetPair();
    int     ___equalSetPair(int8_t *ptr1, int8_t* ptr2);
    void    ___insertSetPair(int8_t *ptr, gtuple* x);
//    int     ___sizeSetPair(int8_t *ptr);
    int     ___isElemSetPair(int8_t *ptr, gtuple* x);
    int8_t* ___unionSetPair(int8_t *ptr1, int8_t * ptr2);
    int8_t* ___intersectSetPair(int8_t *ptr1, int8_t * ptr2);
    int8_t* ___differenceSetPair(int8_t *ptr1, int8_t * ptr2);
    int     ___includesSetPair(int8_t* ptr1, int8_t* ptr2);
    int     ___includesSSetPair(int8_t* ptr1, int8_t* ptr2);
    void    ___freeSetPair(int8_t* ptr);

    /* Multiset */
    int8_t* ___newMultiset();
    int     ___equalMultiset(int8_t* ptr1, int8_t* ptr2);
    void    ___insertMultiset(int8_t* ptr, t x);
    int     ___isElemMultiset(int8_t* ptr, t x);
    int     ___sizeMultiset(int8_t* ptr);
    int     ___countMultiset(t x, int8_t* ptr);
    int8_t* ___unionMultiset(int8_t* ptr1, int8_t* ptr2);
    int8_t* ___sumMultiset(int8_t* ptr1, int8_t* ptr2);
    int8_t* ___intersectMultiset(int8_t* ptr1, int8_t* ptr2);
    int8_t* ___differenceMultiset(int8_t* ptr1, int8_t* ptr2);
    int     ___includesMultiset(int8_t* ptr1, int8_t* ptr2);
    void    ___freeMultiset(int8_t* ptr);

    /* MultisetPair */
    int8_t* ___newMultisetPair();
    int     ___equalMultisetPair(int8_t* ptr1, int8_t* ptr2);
    void    ___insertMultisetPair(int8_t* ptr, gtuple* x);
    int     ___isElemMultisetPair(int8_t* ptr, gtuple* x);
//    int     ___sizeMultisetPair(int8_t* ptr);
    int     ___countMultisetPair(gtuple* x, int8_t* ptr);
    int8_t* ___unionMultisetPair(int8_t* ptr1, int8_t* ptr2);
    int8_t* ___sumMultisetPair(int8_t* ptr1, int8_t* ptr2);
    int8_t* ___intersectMultisetPair(int8_t* ptr1, int8_t* ptr2);
    int8_t* ___differenceMultisetPair(int8_t* ptr1, int8_t* ptr2);
    int     ___includesMultisetPair(int8_t* ptr1, int8_t* ptr2);
    void    ___freeMultisetPair(int8_t* ptr);

    /* Function */
    int8_t* ___newFunction();
    int     ___equalFunction(int8_t* ptr1, int8_t* ptr2);
    void    ___insertFunction(int8_t* ptr, t key, t value);
    int     ___sizeFunction(int8_t *ptr);
    int     ___isElemFunction(int8_t* ptr, t key, t value);
    int8_t* ___funcFromSet(int8_t* setPtr, char* filePath, int line, int col);
    int8_t* ___domFunction(int8_t* ptr);
    int8_t* ___codomainRelation(int8_t *ptr);
    t       ___pairFunction(int8_t* ptr, t k, char* filePath, int line, int col);
    int8_t* ___compositionFunction(int8_t *ptr1, int8_t *ptr2);
    int8_t* ___toSetFunction(int8_t* ptr1);
    int8_t* ___unionFunction(int8_t* ptr1, int8_t* ptr2, char* filePath, int line, int col);
    int8_t* ___intersectFunction(int8_t* ptr1, int8_t* ptr2);
    int8_t* ___differenceFunction(int8_t* ptr1, int8_t* ptr2);
    void    ___freeFunction(int8_t* ptr);

    /* Relation */
    int8_t* ___newRelation();
    int     ___equalRelation(int8_t* ptr1, int8_t* ptr2);
    void    ___insertRelation(int8_t* ptr, t key, t value);
    int     ___sizeRelation(int8_t *ptr);
    int     ___isElemRelation(int8_t* ptr, t key, t value);
    int8_t* ___relationFromSet(int8_t* setPtr);
    int8_t* ___domRelation(int8_t* ptr);
    int8_t* ___codomainRelation(int8_t *ptr);
    int8_t* ___pairRelation(int8_t* ptr, t key);
    int8_t* ___compositionRelation(int8_t *ptr1, int8_t *ptr2);
    void    ___freeRelation(int8_t* ptr);

    /* Sequence */
    int8_t* ___newSequence();
    int     ___equalSequence(int8_t* ptr1, int8_t* ptr2);
    int     ___isElemSequence(int8_t* ptr, t x);
    void    ___insertSequence(int8_t* ptr, t x);
    int8_t* ___concatSequence(int8_t* ptr1, int8_t* ptr2);
    int     ___countSequence(t x, int8_t* ptr);
    int     ___sizeSequence(int8_t* ptr);
    t       ___atSequence(int8_t*ptr, int pos, char* filePath, int line, int col);
    void    ___freeSequence(int8_t* ptr);

    /* SequencePair */
    int8_t* ___newSequencePair();
    int     ___equalSequencePair(int8_t* ptr1, int8_t* ptr2);
    int     ___isElemSequencePair(int8_t* ptr, gtuple* x);
    void    ___insertSequencePair(int8_t* ptr, gtuple* x);
    int8_t* ___concatSequencePair(int8_t* ptr1, int8_t* ptr2);
    int     ___countSequencePair(gtuple* x, int8_t* ptr);
//    int     ___sizeSequencePair(int8_t* ptr);
    gtuple  ___atSequencePair(int8_t*ptr, int pos, char* filePath, int line, int col);
    void    ___freeSequencePair(int8_t* ptr);

    /* Tuple */
    int ___equalTuple(gtuple* x, gtuple* y);


    /*  TrashCollector (Yet Another Garbage Collector)
        
        Every pointer created (set, multiset, ...) is
        stored inside a vector of pointers, to be freed 
        when freeTrashCollector() is called.
     */
    void ___initTrashCollector();
    void ___openScope();
    void ___closeScope();
    void ___freeTrashCollector();


    /* Dynamic Memory Verifications (DMV) 
    
       Every pointer allocated by the programmer is store in
       a set until it is freed. The DMV is need to check 
       invalid memories accesses (e.g. A recent freed pointer 
       beign accessed: free(p); write(*p) ).
    */
    void ___addPointer   (int8_t* ptr);
    void ___removePointer(int8_t* ptr, char* filePath, int l, int c);
    void ___derefPointer (int8_t* ptr, int pragma, char* filePath, int l, int c);

    char *___readln(int **i);

#ifdef __cplusplus
}
#endif
#endif /* Header_h */
