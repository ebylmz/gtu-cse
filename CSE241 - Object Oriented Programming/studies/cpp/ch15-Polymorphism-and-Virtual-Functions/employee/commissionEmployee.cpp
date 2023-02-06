#include <iostream>
#include <string>
#include "commissionEmployee.h"

using std::string;
using std::cerr;
using std::cout;

namespace GTUEmployee {
    CommissionEmployee::CommissionEmployee (const string & firstName, const string & lastName, const string & ssn, double grossSales, double commissionRate)
    : Employee(firstName, lastName, ssn) {
        setGrossSales(grossSales);
        setCommissionRate(commissionRate);            
    }

    void CommissionEmployee::setCommissionRate (double commissionRate) {
        _commissionRate = (commissionRate > 0.0 && commissionRate < 1.0 ) ? commissionRate : 0.0;
    }

    void CommissionEmployee::setGrossSales (double grossSales) {
        _grossSales = (grossSales < 0.0 ) ? 0.0 : grossSales;
    }

    double CommissionEmployee::getCommissionRate () const {return _commissionRate;}
    double CommissionEmployee::getGrossSales () const {return _grossSales;}

    double CommissionEmployee::earnings() const {
        return getCommissionRate() * getGrossSales();
    }

    void CommissionEmployee::print() const {
        cout << "commission employee: ";
        Employee::print(); // code reuse
        cout << "\ngross sales: " << getGrossSales() << "; commission rate: " << getCommissionRate();    }   
}