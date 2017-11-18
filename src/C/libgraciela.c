/* 
  libgraciela.h

  Created by Joel Araujo & José Luis Jiménez (Version Alpha 1.0)
  Modified and maintained by Carlos Spaggiari & Moises Ackerman (Version Beta 2.0)


  How to compile:
  $ cd <GRACIELA_FOLDER_PATH>
  $ clang -O3 -lm -lstdc++ -fPIC -shared src/C/libgraciela-abstract/libgraciela-abstract.cpp src/C/libgraciela.c -o libgraciela.so
*/

#include "math.h"
#include "stdio.h"
#include "limits.h"
#include "stdlib.h"
#include "wchar.h"
#include "locale.h"
#include "string.h"
#include "libgraciela-abstract/libgraciela-abstract.h"


#define MAX_LINE 2048

int8_t* ___openFile(char* name) {
  FILE* file;

  file = fopen(name, "r");
  if(file == NULL)
  {
    printf ("\x1B[0;31mError:\x1B[m Could not open file `%s`\n", name);
    exit(EXIT_FAILURE);
  }
  return (int8_t *) file;
}

void ___closeFile(int8_t* file) {
  FILE* f = (FILE*) file;
  fclose(f);
}

int8_t* ___malloc(int size){  
  int8_t* p = calloc(1,size);
  if (!p) {
    printf("\x1B[0;31mError:\x1B[m Out of memory.");
    exit(EXIT_FAILURE);
  }
  // Mark and add p to the Dynamic Memory Verification (DMV) set
  ___addPointer(p);
  return p;

}

int ___isNan(double x){ return isnan(x); }
int ___isInf(double x){ return isinf(x); }

void ___free(int8_t *mem, char *filePath, int l, int c){
  // Before a piece of memory can be freed, it has to be removed from the DMV set
  ___removePointer(mem, filePath, l ,c);
  free(mem);
}

int ___readFileInt(int8_t* file) {

  FILE* f = (FILE*) file;

  int n;
  int r = fscanf(f, "%d", &n);

  if (r == EOF)
  {
    printf ("\x1B[0;31mError:\x1B[m End of file reached while reading a file\n");
    exit(EXIT_FAILURE);
  }
  if (r == 0)
  {
    printf ("\x1B[0;31mError:\x1B[m The value read from file is not of type \x1B[0;32mint\x1B[m\n");
    exit(EXIT_FAILURE);
  }

  return n;

}

int ___readFileBool(int8_t* file) {
  FILE* f = (FILE*) file;

  char str[64];
  int r = fscanf(f, "%s", str);

  if (r == EOF)
  {
    printf ("\x1B[0;31mError:\x1B[m End of file reached while reading a file\n");
    exit(EXIT_FAILURE);
  }
  if (r == 0)
  {
    printf ("\x1B[0;31mError:\x1B[m The value read from file is not of type \x1B[0;32mboolean\x1B[m\n");
    exit(EXIT_FAILURE);
  }

  if (!strcmp(str,"1")){
    return 1;
  } else if (!strcmp(str,"0")){
    return 0;
  } else if (!strcmp(str,"true")){
    return 1;
  } else if (!strcmp(str,"false")){
    return 0;
  }
  else {
    printf ("\x1B[0;31mError:\x1B[m The value \"%s\" is not of type \x1B[0;32mboolean\x1B[m\n", str);
    exit(EXIT_FAILURE);
    return 0;
  }

}

char ___readFileChar(int8_t* file) {
  FILE* f = (FILE*) file;
  char n;
  int r = fscanf(f, "%c", &n);

  return n;
}

double ___readFileDouble(int8_t* file) {
  FILE* f = (FILE*) file;
  char str[64];
  int r = fscanf(f, "%s", str);

  if (r == EOF)
  {
    printf ("\x1B[0;31mError:\x1B[m End of file reached while reading a file\n");
    exit(EXIT_FAILURE);
  }

  int s = 0;
  int dot = 0;
  int e = 0;
  if (str[0] == '-') ++s;
  for (int i = s ; i < strlen(str) ; ++i){
    if (str[i] == '.') {
      ++dot;
      ++i;
      if (str[i] == '\0'){
        printf ("\x1B[0;31mError:\x1B[m The value \"%s\" is not of type \x1B[0;32mfloat\x1B[m\n", str);
        exit(EXIT_FAILURE);
      }
    }
    if (str[i] == 'e') {
      e++;
      ++i;
      if (str[i] == '-') ++i;
      if (str[i] == '\0'){
        printf ("\x1B[0;31mError:\x1B[m The value \"%s\" is not of type \x1B[0;32mfloat\x1B[m\n", str);
        exit(EXIT_FAILURE);
      }
    }
    if ( (str[i] < '0' || str[i] > '9') || dot > 1 || e > 1) {
      printf ("\x1B[0;31mError:\x1B[m The value \"%s\" is not of type \x1B[0;32mfloat\x1B[m\n", str);
      exit(EXIT_FAILURE);
    }
  }
  return (double)atof(str);
}

void ___lineFeed(){
  char end = (char)getchar();
  if ( end != '\n' && end != '\0'){
    // ASCII 10 "Line Feed"
    ungetc(end, stdin); // put back the next character if its not an \n or \0
  }
}

int ___readIntStd () {
  
  char str[64];
  scanf("%s", str);
  ___lineFeed();

  int s = 0;
  if (str[0] == '-') ++s;
  for (int i = s ; i < strlen(str) ; ++i){
    if (str[i] < '0' || str[i] > '9') {
      printf ("\x1B[0;31mError:\x1B[m The value \"%s\" is not of type \x1B[0;32mint\x1B[m\n", str);
      exit(EXIT_FAILURE);
    }
  }
  return (int)strtol(str,NULL,10);
}

int ___readBoolStd () {
  char str[64];
  scanf("%s", str);
  ___lineFeed();
  if (!strcmp(str,"1")){
    return 1;
  } else if (!strcmp(str,"0")){
    return 0;
  } else if (!strcmp(str,"true")){
    return 1;
  } else if (!strcmp(str,"false")){
    return 0;
  }
  else {
    printf ("\x1B[0;31mError:\x1B[m The value \"%s\" is not of type \x1B[0;32mboolean\x1B[m\n", str);
    exit(EXIT_FAILURE);
    return 0;
  }
}

char ___readCharStd () {
  char n   = (char)getchar();
  
  if (n == '\0' || n == '\n'){
    return n;
  }
  
  ___lineFeed();
  return n;
}




double ___readDoubleStd () {
  char str[64];
  scanf("%s", str);
  ___lineFeed();
  int s = 0;
  int dot = 0;
  int e = 0;
  if (str[0] == '-') ++s;
  for (int i = s ; i < strlen(str) ; ++i){
    if (str[i] == '.') {
      ++dot;
      ++i;
      if (str[i] == '\0'){
        printf ("\x1B[0;31mError:\x1B[m The value \"%s\" is not of type \x1B[0;32mfloat\x1B[m\n", str);
        exit(EXIT_FAILURE);
      }
    }
    if (str[i] == 'e') {
      e++;
      ++i;
      if (str[i] == '-') ++i;
      if (str[i] == '\0'){
        printf ("\x1B[0;31mError:\x1B[m The value \"%s\" is not of type \x1B[0;32mfloat\x1B[m\n", str);
        exit(EXIT_FAILURE);
      }
    }
    if ( (str[i] < '0' || str[i] > '9') || dot > 1 || e > 1) {
      printf ("\x1B[0;31mError:\x1B[m The value \"%s\" is not of type \x1B[0;32mfloat\x1B[m\n", str);
      exit(EXIT_FAILURE);
    }
  }
  return (double)atof(str);
}

void ___writeInt(int x) {
  printf("%d", x);
}


void ___writeDouble(double x) {
  printf("%f", x);
}


void ___writeBool(int x) {
  if (x == 0)
    printf("false");
  else
    printf("true");
}


void ___writeChar(int x) {
  setlocale(LC_CTYPE, "");

  printf("%lc", x);
}

void ___writePointer(int8_t* x) {

  printf("%p", x);
}

void ___writeString(char *x) {
  int i=0;
  setlocale(LC_CTYPE, "");

  printf ("%s", x);
}


void ___ln() {
  printf("\n");
}


void ___randInt(int8_t *x) {
  int *i = (int*)x;
  *i = rand();
}

void ___randFloat(int8_t *x){
  double *f = (double*)x;
  *f = rand()/(double)(RAND_MAX);
}

void ___randChar(int8_t *x){
  char c = (rand() % (CHAR_MAX - CHAR_MIN)) + CHAR_MIN;
  *x = c;
}

void ___randBool(int8_t *x){
  uint8_t b;
  b = rand()%2;
  b = b < 0 ? (-b) : b;
  *x = b;
}


int ___max(int x, int y) {
  return x > y ? x : y;
}


double ___maxF(double x, double y) {
  return x > y ? x : y;
}


int ___min(int x, int y) {
  return x < y ? x : y;
}


double ___minF(double x, double y) {
  return x < y ? x : y;
}

void ___copyArray(int size, int8_t* source, int8_t* dest, int sizeT){
  for (int i = 0; i < size*sizeT; ++i)
    *(dest + i) = *(source + i);
}

typedef enum
  { A_IF
  , A_MANUAL
  , A_POST
  , A_ASSERT
  , A_INVARIANT
  , A_NONDECREASING_BOUND
  , A_NEGATIVE_BOUND
  , A_DIVISION_BY_ZERO
  , A_OVERFLOW
  , A_UNDERFLOW
  , A_EMPTY_RANGE
  , A_NULL_POINTER_ACCESS
  , A_REPRESENTATION_INVARIANT
  , A_NEGATIVE_INDEX
  , A_OUT_OF_BOUNDS_INDEX
  , A_BAD_ARRAY_ARG
  , A_NEGATIVE_ROOT
  , A_NEGATIVE_EXPONENT
  , A_BAD_ABSTRACT_COUPLE
  , A_COUPINVARIANT
  , A_ABSTRACT_POST
  } abort_t;

void ___abort (abort_t reason, char *filePath, int line, int column) {
  printf ("\n\x1B[0;31mABORT:\x1B[m %s at line %d, column %d", filePath, line, column);
  switch (reason) {
    case A_IF:
      printf (":\n\tno true branch found in conditional.\n"); break;
    case A_MANUAL:
      printf (".\n"); break;
    case A_POST:
      printf (":\n\tthe postcondition was falsified.\n"); break;
    case A_ASSERT:
      printf (":\n\tthe assertion was falsified.\n"); break;
    case A_INVARIANT:
      printf (":\n\tthe invariant was falsified.\n"); break;
    case A_NONDECREASING_BOUND:
      printf (":\n\tthe bound didn't decrease.\n"); break;
    case A_NEGATIVE_BOUND:
      printf (":\n\tthe bound became negative.\n"); break;
    case A_DIVISION_BY_ZERO:
      printf (":\n\ta division by zero was attempted.\n"); break;
    case A_OVERFLOW:
      printf (":\n\ta value overflowed.\n"); break;
    case A_UNDERFLOW:
      printf (":\n\ta value underflowed.\n"); break;
    case A_EMPTY_RANGE:
      printf (":\n\tthe quantification range was empty.\n"); break;
    case A_NULL_POINTER_ACCESS:
      printf (":\n\ta null pointer was dereferenced.\n"); break;
    case A_REPRESENTATION_INVARIANT:
      printf (":\n\tthe representation invariant was falsified.\n"); break;
    case A_NEGATIVE_INDEX:
      printf (":\n\ta negative index was used to access an array.\n"); break;
    case A_OUT_OF_BOUNDS_INDEX:
      printf (":\n\tout of bounds index in access to array.\n"); break;
    case A_BAD_ARRAY_ARG:
      printf (":\n\tbad array argument, size mismatch.\n"); break;
    case A_NEGATIVE_ROOT:
      printf (":\n\tattempted to take the square root of a negative value.\n"); break;
    case A_NEGATIVE_EXPONENT:
      printf (":\n\tattempted to raise a number to a negative exponent.\n"); break;
    case A_BAD_ABSTRACT_COUPLE:
      printf (":\n\tthe abstract precondition was falsified.\n\tThe procedure doesn't implement the abstract type.\n"); break;
    case A_COUPINVARIANT:
      printf (":\n\tthe coupling invariant was falsified.\n"); break;
    case A_ABSTRACT_POST:
      printf (":\n\tthe abstract postcondition was falsified.\n"); break;
    default:
      printf (":\n\tunknown reason <%d>.\n", reason); break;
  }
  printf("\n");
  ___freeTrashCollector();
  exit (EXIT_FAILURE);
}

typedef enum
  { W_MANUAL
  , W_PRE
  , W_POST
  , W_INVARIANT
  , W_REPINVARIANT
  , W_COUPINVARIANT
  } warning_t;

void ___warn (warning_t reason, char *filePath, int line, int column) {
  printf ("\x1B[0;35mWARNING:\x1B[m at line %d, column %d", line, column);
  switch (reason) {
    case W_MANUAL:
      printf (".\n"); break;
    case W_PRE:
      printf (":\n\tthe precondition was falsified.\n"); break;
    case W_POST:
      printf (":\n\tthe postcondition was falsified.\n"); break;
    case W_INVARIANT:
      printf (":\n\tthe invariant was falsified.\n"); break;
    case W_REPINVARIANT:
      printf (":\n\tthe representation invariant was falsified.\n"); break;
    case W_COUPINVARIANT:
      printf (":\n\tthe coupling invariant was falsified.\n"); break;
    default:
      printf (":\n\tunknown reason.\n"); break;
  }
}

int ___abs_i (int x, char *filePath, int line, int column) {
  if (x < 0){
    if (x == INT_MIN)
      ___abort (A_OVERFLOW, filePath,  line, column);
    return (-x);
  } else {
    return x;
  }
}

double ___abs_f (double x) {
  return x < 0 ? (-x) : x;
}

int ___sqrt_i (int x, char *filePath, int line, int column) {
  if (x < 0)
    ___abort (A_NEGATIVE_ROOT, filePath,  line, column);
  return floor(sqrt(x));
}

double ___sqrt_f (double x, char *filePath, int line, int column) {
  if (x < 0)
    ___abort (A_NEGATIVE_ROOT, filePath,  line, column);
  return sqrt(x);
}


int ___powInt (int x, int y, char *filePath, int line, int column) {
  if (y < 0)
    ___abort (A_NEGATIVE_EXPONENT, filePath,  line, column);
  else if (y == 0)
    return 1;
  else if (y % 2 == 1) {
    int tmp;
    if (__builtin_smul_overflow(x, ___powInt (x, y-1, filePath, line, column), &tmp))
      // overflow
      ___abort (A_OVERFLOW, filePath,  line, column);
    // all ok
    return tmp;
  } else {
    int tmp = ___powInt (x, y/2, filePath, line, column);
    int tmp2;

    if (__builtin_smul_overflow(tmp, tmp, &tmp2))
      // overflow
      ___abort (A_OVERFLOW, filePath,  line, column);
    // all ok
    return tmp2;
  }
  return 0;
}


int ___float2int (double x, char *filePath, int line, int column) {
  if (-2147483648.49 > x || x > 2147483647.49)
    ___abort (A_OVERFLOW, filePath,  line, column);
  return (x >= 0 ? (int)(x+0.5) : (int)(x-0.5));
}

int ___char2int (char x) {
  return (int)(x);
}

char ___float2char (double x, char *filePath, int line, int column) {
  if (0.0 > x || x > 255.49)
    ___abort (A_OVERFLOW, filePath,  line, column);
  return (char)(x+0.5);
}

char ___int2char (int x, char *filePath, int line, int column) {
  if (0 > x || x > 255)
    ___abort (A_OVERFLOW, filePath,  line, column);
  return (char)x;
}

double ___char2float (char x) {
  return (double)(x);
}

double ___int2float (int x) {
  return (double)(x);
}


int ___traceInt (int x) {
  printf("TRACE: %i\n", x);
  return x;
}
double ___traceFloat (double x) {
  printf("TRACE: %f\n", x);
  return x;
}
char ___traceChar (char x) {
  printf("TRACE: %c\n", x);
  return x;
}
int ___traceBool (int x) {
  printf("TRACE: %s\n", x ? "true" : "false");
  return x;
}
int ___traceStringInt (char* x, int y) {
  printf("TRACE: (%s, %i)\n", x, y);
  return y;
}
double ___traceStringFloat (char* x, double y) {
  printf("TRACE: (%s, %f)\n", x, y);
  return y;
}
char ___traceStringChar (char* x, char y) {
  printf("TRACE: (%s, %c)\n", x, y);
  return y;
}
int ___traceStringBool (char* x, int y) {
  printf("TRACE: (%s, %s)\n", x, y ? "true" : "false");
  return y;
}

