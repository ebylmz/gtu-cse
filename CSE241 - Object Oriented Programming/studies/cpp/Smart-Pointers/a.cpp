#include <iostream>
#include <memory>
#include <vector>

using std::shared_ptr;

int main (void) {
    shared_ptr<int> p1(new int[10]);
    shared_ptr<int> p2 = p1;
    shared_ptr<int> p3 = p1;

    std::cout << p1.reset() 
    std::cout << p1.use_count() << std::endl;
}