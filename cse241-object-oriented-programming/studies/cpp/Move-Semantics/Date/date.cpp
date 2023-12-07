#include <iostream>
#include "date.h"

using std::cout;
using std::cerr;
using std::endl;

Date::Date (int y, int m, int d)
: _year(y), _month(m), _day(d), _sep('/') {
    if (!(1 <= _day && _day <= 31 && 1 <= _month && _month <= 12 && _year >= 0)) {
        cerr << "Invalid date!. Aborted.\n";
        exit(1);
    }
}   
void Date::set (int y, int m, int d) {
    if (!(1 <= _day && _day <= 31 && 1 <= _month && _month <= 12 && _year >= 0)) {
        _year = y;
        _month = m;
        _year = y;
    }
}

int Date::year () const {return _year;}
int Date::month () const {return _month;} 
int Date::day () const {return _day;}

void Date::hu () {
    _sep = '.';
    p1 = &Date::_year;
    p2 = &Date::_month;
    p3 = &Date::_day;
}

void Date::us () {
    _sep = '/';
    p1 = &Date::_month;
    p2 = &Date::_day;
    p3 = &Date::_year;
}

void Date::print (std::ostream & os) const {
    //! UNDERSTAND THE USAGE
    //! DATA MEMBER PONTERS: this + mptr
    os << this->*p1 << _sep << this->*p2 << _sep << this->*p3;
}

ostream & operator<< (ostream & os, const Date & d) {
    d.print(os);
    return os;
}
