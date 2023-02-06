#include <iostream>
#include <string>

using std::string;

// & : lvalue reference
// &&: rvalue reference

class X {};

void f (X & arg);           // lvalue reference parameter
void f (X && arg);          // rvalue reference parameter
void f (const X && arg);    // const lvalue reference parameter

X g ();

string & getName ();

int main (void) {
    X x;
    f(x);       // lvalue parameter f(X &)
    f(g());     // rvalue parameter f(X &&)

    /*-------------*/

    const string & name = getName ();    // OK
    string & name = getName ();          // NOT OK (rvalue cannot be assign to lvalue reference)

    const string && name = getName ();    // OK
    string && name = getName ();          // OK

}

template<class T>
void swap (T & i, T & j) {
    // std::move() converts a value to an rvalue
    T tmp(std::move(a));
    a = std::move(b);
    b = std::move(tmp); 
}