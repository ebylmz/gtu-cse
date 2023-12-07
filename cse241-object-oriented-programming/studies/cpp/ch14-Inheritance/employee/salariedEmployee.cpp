#include <iostream>
#include "employee.h"
#include "salariedEmployee.h"

using std::cerr;
using std::cout;
using std::endl;
using std::string;

namespace EmployeeGTU {
    SalariedEmployee::SalariedEmployee () 
    : Employee(), _salary(0) {}

    SalariedEmployee::SalariedEmployee (const string & name, const string & ssn, double weeklySalary)
    : Employee(name, ssn), _salary(weeklySalary) {} 

    double SalariedEmployee::getSalary () const {return _salary;}
    void SalariedEmployee::setSalary (double salary) {_salary = salary;}

    // printCheck is not const because of setNetPay   
    void SalariedEmployee::printCheck() {
        setNetPay(getSalary());
        cout << "\n________________________________________________\n"
             << "Pay to the order of " << getName() << endl
             << "The sum of " << getNetPay() << " Dollars\n"
             << "_________________________________________________\n"
             << "Check Stub NOT NEGOTIABLE \n"
             << "Employee Number: " << getSsn() << endl
             << "Salaried Employee. Regular Pay: " << getSalary() << endl
             << "_________________________________________________\n";
    }
}