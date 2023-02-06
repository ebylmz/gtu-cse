// https://stackoverflow.com/questions/19429360/what-happens-if-i-dont-catch-a-throw

// When an exception is thrown, it is caught by the topmost function in
// the call stack that has a handler for that exception in the scope of 
// execution. Since you are going back to a function lower in the stack, 
// some variables in the scope of the functions in the upper stack frames
// need to get out of scope, and therefore their destructors are called. 
// This is called stack unwinding. It is really nice to combine that and
// RAII (lookup RAII if you don't know what that is). However, If any 
// destructor throws an exception during stack unwinding, then it is bad 
// and the std::terminate function will be called. Typically your program 
// will then end (and this is why you are always advised to write non-throwing destructors).

#include <iostream>

void f1 () {
    throw 1;
}

void f2 () {
    f1();
}

int main (void) {
    try {
        f2();
    }
    catch (...) {
        std::cout << "caught!"; 
    }
}