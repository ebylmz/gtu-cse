#include <iostream>

void hello(int n) {
    std::cout << "Hello " << n << "!" << std::endl;
}

void hello(const char * s) {
    std::cout << "Hello " << s << "!\n";
}

int main(void) {
    // function pointers are keep the address of function location in the memory
    // auto f1 = hello;
    void (*f2)(int) = hello;
    void (*f3)(const char *) = hello;

    // f1(17);
    f2(12);    
    f3("EBY");
}