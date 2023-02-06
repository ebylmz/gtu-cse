#include <iostream>
using std::cout;
#include "employee.h"

namespace GTUEmployee {
    Employee::Employee (const string & firstName, const string & lastName, const string ssn)
    : _firstName(firstName), _lastName(lastName), _ssn(ssn) {}

    void Employee::setFirstName (const string & firstName) {_firstName = firstName;}
    void Employee::setLastName (const string & lastName) {_lastName = lastName;}
    void Employee::setSsn (const string & ssn) {_ssn = ssn;}

    string Employee::getFirstName () const {return _firstName;}
    string Employee::getLastName () const {return _lastName;}
    string Employee::getSsn () const {return _ssn;}

    void Employee::print() const {
        cout << getFirstName() << ' ' << getLastName()
             << "\nsocial security number: " << getSsn();
    }
}