#include <iostream>
#include <algorithm>
#include <vector>
#include <functional>
#include <iterator>

// both of them okay for my_find_if function
// std::function<bool(int)> func;
// bool(*func)(int);

std::vector<int>::iterator my_find_if (std::vector<int>::iterator it_begin, std::vector<int>::iterator it_end, bool(*func)(int)) {  
    while (it_begin != it_end)
        if (func(*it_begin))
            return it_begin;
        else
            ++it_begin;
    return it_end;    
    
}

int main(void) {
    std::vector<int> v = {2, 4, 8, 16, 32, 64};

    // std::find_if uses given function, if it's return true than 
    auto it1 = std::find_if(v.begin(), v.end(), [] (int val) {return val > 17;});
    if (it1 != v.end())
        std::cout << *it1 << std::endl;
    else
        std::cout << "No element\n";

    auto lambda = [] (int val) {return val > 17;};
    auto it2 = my_find_if(v.begin(), v.end(), lambda); 
    if (it1 != v.end())
        std::cout << *it1 << std::endl;
    else
        std::cout << "No element\n";

}