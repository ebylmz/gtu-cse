#ifndef SALARIED_H
#define SALARIED_H

#include <iostream>
#include <string>
#include "employee.h"

using std::string;

namespace GTUEmployee {
    class SalariedEmployee : public Employee {
    public:
        SalariedEmployee (const string & firstName, const string & lastName, const string ssn, double weeklySalary);
        
        void setWeeklySalary (double weeklySalary);
        
        double getWeeklySalary () const;

        virtual double earnings () const;
        virtual void print () const;
    private:
        double _weeklySalary;
    };
}

#endif