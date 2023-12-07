#include <iostream>
#include <string>
#include "hourlyEmployee.h"
using std::cout;
using std::string;

namespace GTUEmployee {
    HourlyEmployee::HourlyEmployee (const string & firstName, const string & lastName, const string ssn, double wage, double hours) 
    : Employee(firstName, lastName, ssn) {
        setWage(wage);
        setHours(hours);
    }

    void HourlyEmployee::setWage (double wage) {
        _wage = (wage < 0.0) ? 0.0 : wage;
    } 
    void HourlyEmployee::setHours (double hours) {
        _hours = (hours < 0.0 || hours > 168.0) ? 0.0 : hours;

    } 

    double HourlyEmployee::getWage () const {return _wage;} 
    double HourlyEmployee::getHours () const {return _hours;} 

    double HourlyEmployee::earnings () const {
        if (getHours() <= 40.0) // no overtime
            return getWage() * getHours();
        else
            return 40 * getWage() + ( ( getHours() - 40 ) * getWage() * 1.5 );
    }

    void HourlyEmployee::print () const {
        cout << "hourly employee: ";
        Employee::print(); // code reuse
        cout << "\nhourly wage: " << getWage() << "; hours worked: " << getHours();    
    }
}