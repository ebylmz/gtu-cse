#ifndef SALARIED_EMPLOYEE
#define SALARIED_EMPLOYEE

#include "employee.h"
#include <string>

using std::string;

namespace EmployeeGTU {
    class SalariedEmployee : public Employee {
    public:
        SalariedEmployee ();
        SalariedEmployee (const string & name, const string & ssn, double weeklySalary);

        double getSalary () const;

        void setSalary (double salary);

        void printCheck ();
    private:
        double _salary; // weekly
    };
}

#endif