#include <iostream>
using std::cout;
#include "salariedEmployee.h"

namespace GTUEmployee {
    SalariedEmployee::SalariedEmployee (const string & firstName, const string & lastName, const string ssn, double weeklySalary)
    : Employee(firstName, lastName, ssn) {
        setWeeklySalary(weeklySalary);
    }

    void SalariedEmployee::setWeeklySalary (double weeklySalary) {
        _weeklySalary = (weeklySalary < 0.0) ? 0.0 : weeklySalary;
    }
    
    double SalariedEmployee::getWeeklySalary () const {return _weeklySalary;}

    double SalariedEmployee::earnings () const {
        return _weeklySalary;
    }

    void SalariedEmployee::print() const {
        cout << "salaried employee: ";
        Employee::print(); // reuse abstract base-class print function
        cout << "\nweekly salary: " << getWeeklySalary();
    }
}