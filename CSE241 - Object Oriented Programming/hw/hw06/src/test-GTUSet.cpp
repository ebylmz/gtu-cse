#include <iostream>
#include <string> 
#include <algorithm>
#include "../include/GTUSet.h"


using std::cout;
using std::endl;
using std::string;
using stdGTU::GTUSet;

void test1 ();
void test2 ();
void test3 ();

int main (void) {
    test1();
    // test2();
    // test3();
}

void test3() {
    cout << "================== TEST 3 ==================\n";
    GTUSet<int> s;
    int vsize = 5;
    srand(time(NULL));

    for (int i = 0; i < vsize; ++i)
        s.add(rand() % 100);
    
    cout << "Initial set: " << s << endl;
    
    std::sort(s.begin(), s.end());

    cout << "Sorted set:  " << s << endl;

    cout << "================ TEST DONE =================\n\n";

}

void test2 () {
    cout << "================== TEST 2 ==================\n";
    GTUSet<char> s1;
    s1.add('A');
    s1.add('B');
    s1.add('C');
    s1.add('Z');
    cout << "s1:" << s1 << endl;

    GTUSet<char> s2;
    s2.add('F');
    s2.add('C');
    s2.add('B');
    s2.add('E');
    cout << "s2:" << s1 << endl;

    GTUSet<char> s3 = s1 + s2;
    cout << "s3 = s1 + s2\n";
    cout << "s3: " << s3 << endl;

    GTUSet<char> s4 = s3 - s2;
    cout << "s4 = s3 - s2\n";
    cout << "s4: " << s4 << endl;

    if (s4 == s1)
        cout << "s4 = s1\n";

    cout << "================ TEST DONE =================\n\n";

}

void test1 () {
    cout << "================== TEST 1 ==================\n";
    GTUSet<string> set;
    set.add("EBY");
    set.add("YSA");
    set.add("MKA");
    set.add("JBP");
    set.add("CR");
    set.add("YG");
    set.add("SIA");

    std::sort(set.begin(), set.end());

    cout << "Set content: " << set << endl;

    string str("EBY");
    auto it = set.find(str);
    // auto it = std::find(set.begin(), set.end(), str);
    set.erase(it);

    if (it != set.end())
        cout << str << " finded and deleted\n";
    else
        cout << str << " does not deleted, not in set\n";

    cout << "Set content: " << set << endl;

    cout << "================ TEST DONE =================\n\n";

}