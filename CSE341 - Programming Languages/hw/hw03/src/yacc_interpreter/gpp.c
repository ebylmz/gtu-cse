#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "gpp.h"

SymbolTable * sym_tab_alloc() {
    SymbolTable * t = (SymbolTable *) malloc(sizeof(SymbolTable));
    t->size = 0;
    t->capacity = 1;
    t->entries = (Entry **) malloc(sizeof(Entry *));
    return t;
}

void sym_tab_free(SymbolTable * t) {
    int i;
    Entry ** entries = t->entries;
    for (i = 0; i < t->size; ++i)
        free(entries[i]);
    free(t->entries);
    free(t);
}

void increase_capacity(SymbolTable * t) {
    int i;
    Entry ** old = t->entries;
    
    t->capacity *= 2; 
    t->entries = calloc(t->capacity, sizeof(Entry *));
    for (i = 0; i < t->size; ++i)
        t->entries[i] = old[i];
    free(old);
}

int contains(const SymbolTable * t, SymbolType type, const char * id) {
    int i;
    for (i = 0; i < t->size; ++i)
        if (strcmp(t->entries[i]->id, id) == 0 && t->entries[i]->type == type)
            return i;
    return -1;
}

Entry * sym_tab_get(const SymbolTable * t, SymbolType type, const char * id) {
    int i = contains(t, type, id);
    return i == -1 ? NULL : t->entries[i];
}

Entry * sym_tab_def(SymbolTable * t, SymbolType type, char * id, Valuef value) {
    /* make sure not defined before */
    int i = contains(t, type, id);
    if (i != -1)
        return NULL;

    int size = t->size;
    Entry * new_entry = new_entry = (Entry *) malloc(sizeof(Entry));
    new_entry->value = value;
    new_entry->type = type;
    strcpy(new_entry->id, id);

    if (size == t->capacity)
        increase_capacity(t);
    t->entries[size] = new_entry;
    ++(t->size);

    return new_entry;
}

Entry * sym_tab_set(SymbolTable * t, char * id, Valuef value) {
    int i = contains(t, var_t, id);
    if (i == -1)
        return NULL;
    
    t->entries[i]->value = value;
    return t->entries[i];
}

void sym_tab_print(SymbolTable * t) {
	int i;
    Entry * e;
	
	printf("%-30s%-15s\t%s\n", "SYMBOL", "TYPE", "VALUE");
    for (i = 0; i < t->size; ++i) {
        e = t->entries[i];
		printf("%-30s", e->id);
        switch (e->type) {
            case var_t:
                printf("%-15s\t%df%d", "VARIABLE", e->value.num, e->value.denom);
                break;
            case func_t:
                printf("%-15s\t~", "FUNCTION");
                break;
        }
		printf("\n");
    } 
	printf("\n\n");
}

int gcd(int a, int b) {
    return a == 0 ? b : gcd(b % a, a); 
}

void simplify(Valuef * v) {
    int div = gcd(v->num, v->denom);
    v->num = v->num / div;
    v->denom = v->denom / div;
}

Valuef valuef_add(Valuef v1, Valuef v2) {
    Valuef r;
    r.num = v1.num * v2.denom + v2.num * v1.denom;
    r.denom = v1.denom * v2.denom;
    simplify(&r);
    return r;
}

Valuef valuef_sub(Valuef v1, Valuef v2) {
    Valuef r;
    r.num = v1.num * v2.denom - v2.num * v1.denom;
    r.denom = v1.denom * v2.denom;
    simplify(&r);
    return r;
}

Valuef valuef_mult(Valuef v1, Valuef v2) {
    Valuef r;
    r.num = v1.num * v2.num;
    r.denom = v1.denom * v2.denom;
    simplify(&r);
    return r;
}

Valuef valuef_div(Valuef v1, Valuef v2) {
    Valuef r;
    r.num = v1.num * v2.denom;
    r.denom = v1.denom * v2.num;
    simplify(&r);
    return r;
}

Valuef valuef_convert(char * str) {
    Valuef v;
    char * trav = str;
    while (*trav != 'f') ++trav;
    *trav = '\0'; ++trav; 
    v.num = atoi(str);
    v.denom = atoi(trav);
    --trav; *(trav) = 'f'; 
    return v;
}

Valuef valuef_create(int num, int denom) {
    Valuef v;
    v.num = num;
    v.denom = denom;
    return v;
}

int valuef_eq(Valuef v1, Valuef v2) {
    return v1.num * v2.denom == v2.num * v1.denom;
}

int valuef_gt(Valuef v1, Valuef v2) {
    return v1.num * v2.denom > v2.num * v1.denom;
}
