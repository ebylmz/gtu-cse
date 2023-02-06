#include <iostream>
#include "employee.h"
#include "hourlyEmployee.h"

using std::cerr;
using std::cout;
using std::endl;
using std::string;

namespace EmployeeGTU {
    HourlyEmployee::HourlyEmployee () 
    : Employee(), _wageRate(0), _hours(0) {}

    HourlyEmployee::HourlyEmployee (const string & name, const string & ssn, double wageRate, double workedHours) 
    : Employee(name, ssn), _wageRate(wageRate), _hours(workedHours) {}

    double HourlyEmployee::getWageRate () const {return _wageRate;}
    double HourlyEmployee::getHours () const {return _hours;}

    void HourlyEmployee::setWageRate (double wageRate) {_wageRate = wageRate;}
    void HourlyEmployee::setHours (double workedHours) {_hours = workedHours;}
    
    // printCheck is not const because of setNetPay   
    void HourlyEmployee::printCheck() {
        setNetPay(getHours() * getWageRate());
        cout << "\n__________________________________________\n"
             << "Pay to the order of " << getName() << endl
             << "The sum of " << getNetPay() << " Dollars\n"
             << "__________________________________________\n"
             << "Check Stub: NOT NEGOTIABLE\n"
             << "Employee Number: " << getSsn() << endl
             << "Hourly Employee. \nHours worked: " << getHours()
             << " Rate: " << getWageRate() << " Pay: " << getNetPay( ) << endl
             << "__________________________________________\n";    
    }
}