#ifndef DATE_H
#define DATE_H

#include <iostream>

using std::ostream;

class Date {
public:
    Date (int y = 1, int m = 1, int d = 1);
    
    void set (int y, int m, int d);
    
    int year () const;
    int month () const;
    int day () const;

    void hu ();
    void us ();
    void print (ostream & os) const;
private:
    int _year;
    int _month;
    int _day;

    // for different date format US and HU
    char _sep;
    //! int of class Date pointer
    //! so p1, p2, p3 can point an integer inside Date class 
    int Date::* p1;
    int Date::* p2;
    int Date::* p3;
};

ostream & operator<< (ostream & os, const Date & d);

#endif