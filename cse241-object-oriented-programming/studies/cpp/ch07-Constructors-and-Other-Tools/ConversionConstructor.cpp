// demonstrates usage of explicit keyword and conversion constructor

// https://www.geeksforgeeks.org/use-of-explicit-keyword-in-cpp/
// https://www.geeksforgeeks.org/type-conversion-in-c/
// https://www.geeksforgeeks.org/g-fact-35/

#include <iostream>

class Pair {
public:
    explicit Pair (int first = 0, int second = 0) : __first(first), __second(second) {}

    void setFirst (int val) {__first = val;}
    void setSecond (int val) {__second = val;}

    int first () const {return __first;}
    
    int second () const {return __second;}

    bool operator== (const Pair & o) const {
        return first() == o.first() && second() == o.second();
    }

    bool operator!= (const Pair & o) const {return !(*this == o);}

private:
    int __first;
    int __second;
};

int main(int argc, char const *argv[]) {
    Pair p1(1, 0);
    Pair p2((Pair) {1, 5});


    std::cout << "(p1 == (Pair) 1) :" << ((p1 == (Pair) 1) ? "true" : "false") << std::endl;

    std::cout << "(p1 == static_cast<Pair>(1)) : " << ((p1 == static_cast<Pair>(1)) ? "true" : "false") << std::endl;

    // conversion constructor with {}
    std::cout << "(p1 == (Pair) {1, 0}) : " << ((p1 == (Pair) {1, 0}) ? "true" : "false") << std::endl;
}
