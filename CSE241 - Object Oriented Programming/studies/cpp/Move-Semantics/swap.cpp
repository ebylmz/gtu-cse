#include <iostream>
#include <vector>
using std::vector;

class S {
public:
    S () { a = ++cnt; std::cout << "S() "; }
    S (const S& rhs) { a = rhs.a; std::cout << "copyCtr "; }
    S (S&& rhs) noexcept { a = rhs.a; std::cout << "moveCtr "; }
    S & operator= (const S& rhs) { a = rhs.a; std::cout << "copy= "; return *this; }
    S & operator= (S&& rhs) { a = rhs.a; std::cout << "move= "; return *this; }
    int a;
    static int cnt; // number of constructor calling
};

void test1 ();
void test2 ();
void test3 ();

template<class T>
void swap (T & a, T & b);

int S::cnt = 0;     // initialize static variable

int main (void) {
    /*
    test0();
    test1();
    */
    test2();
}

void test0 () {
    S a, b;
    swap(a, b);
}

void test1 () {
    S s1, s2;
    swap(s1, s2);

    vector<int> v1(100);
    std::cout << "size of v1: " << v1.size() << std::endl;

    vector<int> v2(std::move(v1));
    std::cout << "size of v1: " << v1.size() << std::endl;
    std::cout << "size of v2: " << v2.size() << std::endl;

    v1.push_back(12);
    std::cout << "size of v1: " << v1.size() << std::endl;
}

void test2 () {
    std::vector<S> sv(5);    
    sv.push_back(S());

    for (std::size_t i = 0; i < sv.size(); ++i)
        std::cout << sv[i].a << " ";
    std::cout << std::endl;
}

template<class T>
void swap (T & a, T & b) {
    T tmp(std::move(a));    // move the resource of a to tmp
    a = std::move(b);       // move the resource of b to a
    b = std::move(tmp);     // move the resource of tmp to a
}