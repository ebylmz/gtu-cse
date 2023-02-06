#include <iostream>
#include <functional>

int main (void) {
    int n = 5;
    int m = 0;

    [&, n] (int a) mutable { m = ++n + a; }(4);
    std::cout << "n: " << n << " m: " << m << std::endl;    
}