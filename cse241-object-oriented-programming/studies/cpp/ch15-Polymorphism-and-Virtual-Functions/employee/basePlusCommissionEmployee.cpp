#include <iostream>
#include <string>
#include "basePlusCommissionEmployee.h"

using std::string;
using std::cerr;
using std::cout;

namespace GTUEmployee {
    BasePlusCommissionEmployee::BasePlusCommissionEmployee (const string & firstName, const string & lastName, const string & ssn, double grossSales, double commissionRate, double baseSalary)
    : CommissionEmployee(firstName, lastName, ssn, grossSales, commissionRate) {
        setBaseSalary(baseSalary);
    }

    void BasePlusCommissionEmployee::setBaseSalary (double baseSalary) {
        _baseSalary = (baseSalary > 0.0) ? baseSalary : 0.0;
    }   

    double BasePlusCommissionEmployee::getBaseSalary () const {return _baseSalary;} 

    double BasePlusCommissionEmployee::earnings () const {
        return getBaseSalary() + CommissionEmployee::earnings();
    } 

    void BasePlusCommissionEmployee::print() const {
        cout << "base-salaried ";
        // invoke CommissionEmployee's print function
        CommissionEmployee::print();
        cout << "\nbase salary: " << getBaseSalary();
    } 
}