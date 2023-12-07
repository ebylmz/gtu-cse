#ifndef BASEPLUS_H
#define BASEPLUS_H

#include <iostream>
#include <string>
#include "commissionEmployee.h"

using std::string;

namespace GTUEmployee {
    class BasePlusCommissionEmployee : public CommissionEmployee {
    public:
        BasePlusCommissionEmployee (const string & firstName, const string & lastName, const string & ssn, double grossSales, double commissionRate, double baseSalary);

        void setBaseSalary (double);    
  
        double getBaseSalary () const; 

        virtual double earnings () const;   // calculate earnings
        virtual void print () const;        // print BasePlusCommissionEmployee object
    private:
        double _baseSalary; 
    }; 
} 

#endif