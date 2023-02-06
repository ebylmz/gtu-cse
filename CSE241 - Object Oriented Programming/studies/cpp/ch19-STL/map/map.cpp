#include <iostream>
#include <map>

using std::map;

int main (void) {
    map<char, int> mp = {{'Z', 1}, {'E', 2}, {'A', 3}};
    
    mp.insert(std::pair<char, int>('S', 5));
    
    mp.emplace('B', 7);

    // If k does not match the key of any element in the container, 
    //the function inserts a new element with that key and returns a reference to its mapped value.
    mp['U'] = 4;

    // Internally, the elements in a map are always sorted by its key
    // Maps are typically implemented as binary search trees.

    std::cout << "mp['A'] = " << mp['A'] << std::endl;

    for (map<char, int>::iterator it = mp.begin(); it != mp.end(); ++it)
        std::cout << it->first << "/" << it->second << " ";
    std::cout << std::endl;
}