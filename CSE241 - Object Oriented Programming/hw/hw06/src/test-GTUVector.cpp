#include <iostream>
#include <string> 
#include <algorithm> 

#include "../include/GTUVector.h"

using std::cout;
using std::endl;
using std::string;
using stdGTU::GTUVector;

void test1 ();
void test2 ();

int main (void) {
    test1();
    test2();
}

void test2 () {
    cout << "================== TEST 2 ==================\n";

    GTUVector<string> v(3, "Mary");
    string s[] = {"Jordan", "Yusuf", "Achilles", "Tom"};

    // calls copy pushBack
    for (string & svalue : s)
        v.pushBack(svalue);
    
    // calls move pushBack
    v.pushBack("Mozart");
    v.pushBack("Picasso");
    v.pushBack("Beethoven");

    cout << "Vector: "  << v << endl;

    srand(time(NULL));
    int position = rand() % v.size();
    cout << "v[" << position << "]: " << v[position] << endl;
    
    position = rand() % v.size();
    cout << "v.at(" << position << "): " << v.at(position) << endl;

    for (int i = 0; i < 4; ++i)
        v.popBack();

    cout << "\npopBack 4 times\n" 
         << "Vector: "  << v << endl;

    cout << "================ TEST DONE =================\n\n";
}

void test1 () {
    cout << "================== TEST 1 ==================\n";
    GTUVector<int> v;

    cout << "size: " << v.size() << " capacity: " << v.capacity() << endl;
    if (v.empty()) 
        cout << "Initialy vector is empty\n";

    const int SIZE = 20;
    srand(time(NULL));  
    for (int i = 0; i < SIZE; ++i)
        v.pushBack(rand() % 1000);
    
    cout << "\nVector content: ";
    for (int & nvalue : v)
        cout << nvalue << " ";
    cout << endl << "size: " << v.size() << " capacity: " << v.capacity() << endl;

    std::sort(v.begin(), v.end());
    cout << "\nsort array with std::sort()\n"
         << "Vector: " << v << endl;

    cout << "\nClear the vector\n";
    v.clear();

    for (GTUVector<int>::iterator it = v.begin(); it != v.end(); ++it)
        cout << *it << " ";
    cout << "size: " << v.size() << " capacity: " << v.capacity() << endl;

    cout << "================ TEST DONE =================\n\n";
}