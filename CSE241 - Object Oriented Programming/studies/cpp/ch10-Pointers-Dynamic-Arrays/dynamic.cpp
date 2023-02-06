#include <iostream>

using namespace std;

void test() {
    // The constant NULL is actually the integer 0
    if (NULL == '\0')
        cout << "They have same value but types are different\n";
    else 
        cout << "NOO\n";
    cout << static_cast<char>(static_cast<int>(NULL) + 65); // Letter A     
}

// Pointers as Call-by-Value Parameters
void swap (int * & ptr1, int * & ptr2) {
    int * tmp = ptr1;
    ptr1 = ptr2;
    ptr2 = tmp;
}

int main (void) {
    // Operator new is C++ version of malloc 
    int * p = new int(17);  // Calls default constructor
    cout << "Before operator delete: " << *p << endl;
    delete p;
    cout << "After operator delete : " << *p << endl;
    p = NULL;   
    cout << "Assign NULL it        : " << *p << endl; // Something happen and does not output it
}