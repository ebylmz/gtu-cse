#include <iostream>
#include "employee.h"

using std::cerr;

namespace EmployeeGTU {
    Employee::Employee ()
    : _netPay(0) {}

    Employee::Employee (const string & name, const string & ssn)
    : _name(name), _ssn(ssn), _netPay(0) {}

    string Employee::getName () const {return _name;}
    string Employee::getSsn () const {return _ssn;}
    double Employee::getNetPay () const {return _netPay;}

    void Employee::setName (const string & name) {_name = name;}
    void Employee::setSsn (const string & ssn) {_ssn = ssn;}
    
    void Employee::setNetPay (double netPay) {
        if (netPay < 0)
            cerr << "Payment cannot be negative value\n";
        else
            _netPay = netPay;
    }

    void Employee::printCheck () const {
        cerr << "\nERROR: printCheck FUNCTION CALLED FOR AN \n"
             << "UNDIFFERENTIATED EMPLOYEE. Aborting the program.\n"
             << "Check with the author of the program about this bug.\n";
        exit(1);    
    }
}