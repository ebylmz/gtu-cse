// This is the header file employee.h.
// This is the interface for the class Employee.
// This is primarily intended to be used as a base class to derive
// classes for different kinds of employees.

#ifndef EMPLOYEE_H
#define EMPLOYEE_H

#include <string>
using std::string;

namespace EmployeeGTU {
    class Employee {
    public:
        Employee ();
        Employee (const string & name, const string & ssn);
        
        string getName () const;
        string getSsn () const;
        double getNetPay () const;

        void setName (const string & name);
        void setSsn (const string & ssn);
        void setNetPay (double netPay);

        void printCheck () const;
    private:
        string _name;
        string _ssn;
        double _netPay;
    }; // class Employee
} // namespace EmployeeGTU

#endif