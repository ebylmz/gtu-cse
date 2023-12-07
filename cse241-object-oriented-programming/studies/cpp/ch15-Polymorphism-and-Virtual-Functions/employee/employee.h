#ifndef EMPLOYEEE_H
#define EMPLOYEEE_H

#include <iostream>
#include <string>

using std::string;

namespace GTUEmployee {
    class Employee {
    public:
        // big three does not needed
        //! no default constructor
        Employee (const string & firstName, const string & lastName, const string ssn);

        void setFirstName (const string & firstName);
        void setLastName (const string & lastName);
        void setSsn (const string & ssn);

        string getFirstName () const;
        string getLastName () const;
        string getSsn () const;

        // pure virtual function makes Employee abstract base class
        virtual double earnings () const = 0;   // pure virtual
        virtual void print () const;            // virtual
    private:
        string _firstName;
        string _lastName;
        string _ssn;  // social security number
    };
}

#endif