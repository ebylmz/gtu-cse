#ifndef GPP_H
#define GPP_H

typedef struct Valuef {
    int num;    /* numerator */
    int denom;  /* denominator */
} Valuef;

typedef struct Entry {
    char id[32];
    char type;   
    Valuef value;
} Entry;

typedef struct Node {
    Valuef data;
    struct Node * next;
} Node;

typedef struct SymbolTable {
    Entry ** entries;
    int size;
    int capacity;
} SymbolTable;

typedef enum SymbolType {
    func_t, var_t
} SymbolType; 

SymbolTable * sym_tab_alloc();

void sym_tab_free(SymbolTable * t);

Entry * sym_tab_get(const SymbolTable * t, SymbolType type, const char * id);

Entry * sym_tab_def(SymbolTable * t, SymbolType type, char * id, Valuef value);

Entry * sym_tab_set(SymbolTable * t, char * id, Valuef value);

void sym_tab_print(SymbolTable * t);

Valuef valuef_add(Valuef v1, Valuef v2);

Valuef valuef_sub(Valuef v1, Valuef v2);

Valuef valuef_mult(Valuef v1, Valuef v2);

Valuef valuef_div(Valuef v1, Valuef v2);

Valuef valuef_convert(char * str);

Valuef valuef_create(int num, int denom);

int valuef_eq(Valuef v1, Valuef v2);

int valuef_gt(Valuef v1, Valuef v2);

#endif