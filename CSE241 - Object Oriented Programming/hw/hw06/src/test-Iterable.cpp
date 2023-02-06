#include <iostream>
#include <algorithm> 
#include <string>
#include "../include/Iterable.h"
#include "../include/GTUArray.h"
#include "../include/GTUSet.h"
#include "../include/GTUVector.h"

using std::cout;
using std::endl;
using std::string;
using stdGTU::Iterable;
using stdGTU::GTUVector;
using stdGTU::GTUArray;
using stdGTU::GTUSet;

template<typename T>
void test1 (Iterable<T> & container); 

int main (void) {
    GTUVector<string> v;
    v.pushBack("Zulal"); 
    v.pushBack("Arda"); 
    v.pushBack("Ceren"); 
    v.pushBack("Eda"); 
    v.pushBack("Serkan"); 
    v.pushBack("Rabia"); 
    test1(v);

    GTUSet<int> s;
    s.add(144); 
    s.add(9); 
    s.add(100); 
    s.add(196); 
    s.add(64); 
    s.add(25); 
    s.add(169); 
    s.add(256); 
    s.add(1); 
    test1(s);

    GTUArray<char, 10> a;
    a[0] = 'F';
    a[1] = 'A';
    a[2] = 'F';
    a[3] = 'E';
    a[4] = 'R';
    a[5] = 'M';
    a[6] = 'Z';
    a[7] = 'C';
    a[8] = 'N';
    a[9] = 'I';
    test1(a);
}

template<typename T>
void foo (const T & val) {
    cout << ' ' << val;
}


// requares input_iterator
/*
template<typename T>
void test1 (Iterable<T> & container) {
    cout << "container contains:";
    for_each(container.begin(), container.end(), foo);
    cout << '\n';
}
*/


template<typename T>
void test1 (Iterable<T> & container) {
    cout << "=================== TEST ==================\n";
    cout << "container: " << container << endl;

    std::sort(container.begin(), container.end());
    cout << "sort with std::sort()\n";
    cout << "container: " << container << endl;
    cout << "\n================ TEST DONE =================\n\n";
}