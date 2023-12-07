#include <iostream>
#include <string>
#include <vector>
#include <algorithm>
#include "iter.h"

using std::cout;
using std::endl;
using std::string;
using std::vector;

void test1 ();
void test2 ();

int main (void) {
    test1();
}

void test2 () {
    vector<int> v;
    v.push_back(12);
    v.push_back(13);
    v.push_back(14);

    vector<int>::const_iterator it = v.cbegin();
    // *it = 1;    // read only
}

void test1 () {
    Container<string> c;
    c.push_back("CR7");
    c.push_back("EBY");
    c.push_back("JBP");
    c.push_back("SIA");
    c.push_back("YSA");
    c.push_back("MKA");

    std::sort(c.begin(), c.end());

    for (auto it = c.begin(); it != c.end(); ++it)
        cout << *it << endl;
}