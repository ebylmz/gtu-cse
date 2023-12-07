#include <iostream>
#include <vector>

void printValue (int n) {
    std::cout << n << std::endl;
}

void  forEach (const std::vector<int> & values, void(*func)(int)) {
    for (auto val : values)
        func(val);
}

int main(void) {
    std::vector<int> v = {2, 4, 8, 16, 32, 64};
    
    std::cout << "using function pointer" << std::endl;
    forEach(v, printValue);
    
    std::cout << "using lambda expression" << std::endl;
    forEach(v, [](int n) {std::cout << n << std::endl;});
}