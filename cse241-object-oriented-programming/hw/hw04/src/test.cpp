/**
 * @file    test.cpp
 * @author  Emirkan Burak Yılmaz 
 * @brief   The test/driver program for DayOfYearSet & DayOfYear classes   
 * @version 0.1
 * @date    2021-12-04
 * 
 * @copyright Copyright (c) 2021
 */

#include <iostream>
#include "dayOfYearSet.h"

using namespace std;
using namespace DoYGTU;

void test0 ();
// test function for DayOfYear class
// tests member functions operator++, setDay(), setMonth()
void test1 ();
// test function for DayOfYear class
// tests member fucntions operator==, dayBetween()
void test2 ();
// test function for DayOfYearSet class
// tests member functions add(), remove(), empty() 
// size(), operator==, operator!=, operator[]  
void test3 ();
// test function for DayOfYearSet class
// test union, difference, intersection and compliment properties
// tests member functions operator^, operator+, operator-, operator! 
void test4 ();
// test function for DayOfYearSet class
// test the member functions load() and save() 
void test5 ();
// test function for DayOfYearSet class
// tests the De Morgan Laws
void test6 ();
// test function for DayOfYearSet class
// test the static function AllDoY

int DayOfYearSet::_AllDoY = 0;

int main (void) {
    // test0 and test1 tests inner class DayOfYear
    /*
    cout << "Test0\n" << "================================\n";
    test0();
    cout << "\nTest1\n" << "================================\n";
    test1();
    */
    cout << "\nTest2\n" << "================================\n";
    test2();
    cout << "\nTest3\n" << "================================\n";
    test3();
    cout << "\nTest4\n" << "================================\n";
    test4();
    cout << "\nTest5\n" << "================================\n";
    test5();
    cout << "\nTest6\n" << "================================\n";
    test6();

    cout << "\n============== END OF DRIVER PROGRAM ==============\n";
    return 0;
}

void test0 () {
    DayOfYearSet::DayOfYear d1(12, 31), d2(3, 24);
    cout << "today       : " << d1 << " (last day of the year)\n"
         << "tomarrow    : " << ++d1 << endl
         << "5 days later: " << d1 + 4 << endl
         << "The days passed so far: " << d1.daySoFar() << "\n\n";

    d2.setDay(1);
    d2.setMonth(1);
    cout << "today     : " << d2 << " (first day of the year)\n"
         << "yesterday : " << --d2 << endl

         << "5 days ago: " << d2 - 4 << endl    
         << "The days passed so far: " << d2.daySoFar() << endl;
}

void test1 () {
    DayOfYearSet::DayOfYear today, birthday;
    cout << "Enter todays date (MM/DD): ";
    cin >> today;    
    cout << "Enter your birthday (MM/DD): ";
    cin >> birthday;

    cout << "\nToday is "; 
    today.print();
    cout << "\nYour birthday is ";
    birthday.print();
    cout << "\n";

    if (today == birthday)
        cout << "Happy birthday to youu\n";
    else
        cout << today.dayBetween(birthday) << " days left for your next birthday\n";
}   

void test2 () {
    DayOfYearSet s1, s2;
    DayOfYearSet::DayOfYear days[5] = {DayOfYearSet::DayOfYear(1, 1), 
                                       DayOfYearSet::DayOfYear(3, 12),
                                       DayOfYearSet::DayOfYear(4, 10),
                                       DayOfYearSet::DayOfYear(5, 21),
                                       DayOfYearSet::DayOfYear(5, 17)};

    // construct s1 and s2
    cout << "construct s1 and s2\n";
    s1.add(days[0]);
    s1.add(days[1]);
    s1.add(days[2]);
    s1.add(days[3]);
    s1.add(days[4]);
    cout << "s1: " << s1 << endl;

    // add same days as s1 with different order
    s2.add(days[4]);
    s2.add(days[2]);
    s2.add(days[3]);
    s2.add(days[1]);
    s2.add(days[0]);
    cout << "s2: " << s2 << endl;

    if (s1 == s2)
        cout << "s1 and s2 are equal\n";
    else
        cout << "s1 and s2 are not equal\n";

    // removes some of the elements of s1 than compare with s2
    cout << "\nremove elements " << s1[1] << " and " << days[4] << " from s1\n";
    s1.remove(1);
    s1.remove(days[4]);

    cout << "s1: " << s1 << endl;
    cout << "s2: " << s2 << endl;

    if (s1 != s2)
        cout << "s1 and s2 are not equal\n";
    else
        cout << "s1 and s2 are equal\n";

    cout << "\nMake empty s1 and s2 (delete the sets)\n";
    s1.empty();
    cout << "s1: " << s1 << endl;
    s2.empty();
    cout << "s2: " << s2 << endl;

    if (s1 != s2)
        cout << "s1 and s2 are not equal\n";
    else
        cout << "s1 and s2 are equal\n";
}

void test3 () {
    DayOfYearSet s1, s2;
    
    cout << "construct s1 and s2\n";
    s1.add(DayOfYearSet::DayOfYear(1, 1));
    s1.add(DayOfYearSet::DayOfYear(4, 10));
    s1.add(DayOfYearSet::DayOfYear(5, 21));
    s1.add(DayOfYearSet::DayOfYear(5, 17));
    s1.add(DayOfYearSet::DayOfYear(3, 12));
    s1.add(DayOfYearSet::DayOfYear(5, 29));
    s1.add(DayOfYearSet::DayOfYear(2, 21));
    s1.add(DayOfYearSet::DayOfYear(9, 12));

    s2.add(DayOfYearSet::DayOfYear(4, 22));
    s2.add(DayOfYearSet::DayOfYear(5, 21));
    s2.add(DayOfYearSet::DayOfYear(7, 1));
    s2.add(DayOfYearSet::DayOfYear(3, 12));
    s2.add(DayOfYearSet::DayOfYear(12, 15));
    s2.add(DayOfYearSet::DayOfYear(5, 17));

    cout << "s1: " << s1 << endl
         << "s2: " << s2 << endl;

    // DayOfYearSet unionSet = s1 + s2;
    // DayOfYearSet diffSet = s1 - s2;
    // DayOfYearSet isectionSet = s1 ^ s2;
    // DayOfYearSet s1Complement = !s1;
    // DayOfYearSet s2Complement = !s2;
    
    cout << "\nUnion\n" 
         << "s1 + s2: " << (s1 + s2) << "\n\n";
    cout << "Difference\n" 
         << "s1 - s2: " << (s1 - s2) << "\n\n";
    cout << "Intersection\n" 
         << "s1 ^ s2: " << (s1 ^ s2) << "\n\n";
    cout << "Complement of s1\n" 
         << "!s1    : " << !s1 << "\n\n";
    cout << "Complement of s2\n" 
         << "!s2    : " << !s2 << "\n\n";
    // cout << "Complement of s1 has " << (!s1).size() << " element\n";
}

void test4 () {
    // construct sets s1 and s2
    DayOfYearSet s1;
    s1.add(3, 12);
    s1.add(4, 10);
    s1.add(5, 21);
    s1.add(4, 22);
    s1.add(7, 1);
    s1.add(12, 15);
    cout << "s1: " << s1 << endl;
    s1.save("set1.txt");

    DayOfYearSet s2(s1);
    s2.add(12, 11);
    s2.add(2, 28);
    s2.add(5, 2);
    cout << "s2: " << s2 << endl;
    s2.save("set2.txt");   

    // take difference of s1 and s2 and save the result the file
    DayOfYearSet s3 = s2 - s1;
    cout << "s3: " << s3 << endl;
    s3.save("set3.txt");
    
    cout << "\nSets are printed to the files:\n"
         << "s1: set1.txt\n"
         << "s2: set2.txt\n"
         << "s3: set3.txt\n";
}

// Note that for sets s1 and s2, De Morgan says that !(s1 + s2) == !s1 ^ !s2
void test5 () {
    // load the sets s1 and s2 from the files
    DayOfYearSet s1("setA.txt"), s2("setB.txt");
    int r;

    // cout << "Sets\n"
    //      << "s1: " << s1 << " (size: " << s1.size() << ")\n"
    //      << "s2: " << s2 << " (size: " << s2.size() << ")\n";

    if (!(s1 + s2) == (!s1 ^ !s2)) {
        cout << "The complement of the union of two sets is " 
             << "the same as the intersection of their complements\n";
        if (!(s1 ^ s2) == !s1 + !s2)
            cout << "The complement of the intersection of two sets "
                 << "is the same as the union of their complements\n";
        else
            r = EXIT_FAILURE;
    }
    else 
        r = EXIT_FAILURE;
    
    if (r == EXIT_FAILURE)
        cout << "Either De Morgan was Wrong or I implemented something wrong :)\n";             
}

void test6 () {
    vector<DayOfYearSet::DayOfYear> v;
    v.push_back(DayOfYearSet::DayOfYear(1, 22));
    v.push_back(DayOfYearSet::DayOfYear(2, 23));
    v.push_back(DayOfYearSet::DayOfYear(3, 24));
    v.push_back(DayOfYearSet::DayOfYear(4, 25));
    
    DayOfYearSet s1(v), s2;
    s2.add(12, 2);    
    s2.add(6, 21);    
    s2.add(2, 25);    
    s2.add(7, 29);    
    s2.add(3, 17);    
    cout << "So far " << DayOfYearSet::AllDoY() << " DoY object alive in DayOfYearSet sets\n";

    s1.remove(0);
    s1.remove(0);
    s1.remove(0);

    cout << "Some of the elements are deleted\n"
         << "Current DoY object number is " << DayOfYearSet::AllDoY() << endl; 

    DayOfYearSet s3;
    s3.load("setA.txt");

    cout << s3.size() << " new DoY object created\n"
         << "So right now " << DayOfYearSet::AllDoY() << " object alive in DayOfYearSet sets\n"; 
}