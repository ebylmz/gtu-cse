#ifndef HOURLY_EMPLOYEE
#define HOURLY_EMPLOYEE

#include "employee.h"
#include <string>

using std::string;

namespace EmployeeGTU {
    class HourlyEmployee : public Employee {
    public:
        HourlyEmployee ();
        HourlyEmployee (const string & name, const string & ssn, double wageRate, double workedHours);

        double getWageRate () const;
        double getHours () const;

        void setWageRate (double wageRate);
        void setHours (double workedHours);

        void printCheck ();
    private:
        double _wageRate;
        double _hours;  // worked hours
    };
}

#endif