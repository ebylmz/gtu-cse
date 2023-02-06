#include <iostream>

class SomeClass {
    public:
        int & f ();             // will be used in any-l value invocation
        const int & f () const; // will be used in any-r value invocation
        /* Their signatures are different because const at the end of the decleration */
        // ... 
};