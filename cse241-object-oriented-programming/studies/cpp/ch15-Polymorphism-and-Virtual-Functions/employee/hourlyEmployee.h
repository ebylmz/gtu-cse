#ifndef HOURLY_H
#define HOURLY_H

#include <iostream>
#include <string>
#include "employee.h"

using std::string;

namespace GTUEmployee {
    class HourlyEmployee : public Employee {
    public:
        HourlyEmployee (const string & firstName, const string & lastName, const string ssn, double wage, double hours);
        
        void setWage (double wage); 
        void setHours (double hours); 

        double getWage () const; 
        double getHours () const; 

        virtual double earnings () const;
        virtual void print () const;
    private:
        double _wage;   // wage (payment) per hour
        double _hours; // hours worked for week
    };
}

#endif