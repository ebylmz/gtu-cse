#ifndef COMMISSION_H
#define COMMISSION_H

#include <iostream>
#include <string>
#include "employee.h"

using std::string;

namespace GTUEmployee {
    class CommissionEmployee : public Employee {
    public:
        CommissionEmployee (const string & firstName, const string & lastName, const string & ssn, double grossSales, double commissionRate);

        void setCommissionRate (double commissionRate);
        void setGrossSales (double grossSales);

        double getCommissionRate () const;
        double getGrossSales () const;
        
        virtual double earnings() const; // calculate earnings
        virtual void print() const;      // print CommissionEmployee object
    private:
        double _grossSales;      // gross weekly sales
        double _commissionRate;  // commission percentage
    }; 
} 

#endif