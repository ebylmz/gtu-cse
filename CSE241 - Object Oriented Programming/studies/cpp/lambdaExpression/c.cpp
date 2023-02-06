#include <iostream>
#include <vector>
#include <functional> // std::function required o.w. uses raw pointer for lambdas

void  forEach (const std::vector<int> & values, std::function<void(int)> func) {
    for (auto val : values)
        func(val);
}

int main(void) {
    std::vector<int> v = {2, 4, 8, 16, 32, 64};

    int a = 5;
    // capture a variable with value
    auto lambda1 = [=](int val) {std::cout << a << std::endl;};
    // prints value of 5 for each element in vector 
    forEach(v, lambda1);
    
    const char * str = "Hello World!";
    // capture a variable with referance
    auto lambda2 = [&](int val) {std::cout << str << std::endl;};
    // prints value of str for each element in vector 
    forEach(v, lambda2);

    // capture by value and capture by referance
    std::function<void(int)> lambda3 = [=, &str] (int val) {std::cout << str << "\t" << a << std::endl;};
    forEach(v, lambda3);
}