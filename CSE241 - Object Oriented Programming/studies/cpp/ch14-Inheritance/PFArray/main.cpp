//Program to demonstrate the template class PFArray.
#include <iostream>
#include <string>
using std::cin;
using std::cout;
using std::endl;
using std::string;
#include "pfarray.h"
#include "pfarray.cpp"
using stdEBY::PFArray;

int main () {
    PFArray< int> a(10);
    cout << "Enter up to 10 nonnegative integers.\n";
    cout << "Place a negative number at the end.\n";
    int next;
    cin >> next;

    for (int i = 0; (next >= 0) && (!a.full( )); ++i) {
        a.add(next);
        cin >> next;
    }
    if (next >= 0) {
        cout << "Could not read all numbers.\n";
        //Clear the unread input:
        while (next >= 0)
            cin >> next;
    }
    
    cout << "You entered the following:\n ";
    for (int i = 0; i < a.used(); i++)
        cout << a[i] << " ";
    cout << endl;
    
    PFArray<string> b(3);
    cout << "Enter three words:\n";
    
    string nextWord;
    for (int i = 0; i < 3; i++) {
        cin >> nextWord;
        b.add(nextWord);
    }
    cout << "You wrote the following:\n";
    for (int i = 0; i < b.used(); i++)
        cout << b[i] << " ";
    cout << endl;
    
    cout << "I hope you really mean it.\n";
    return 0;
}