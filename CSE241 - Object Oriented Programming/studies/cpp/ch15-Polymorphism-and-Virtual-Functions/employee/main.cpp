#include <iostream>
using std::cout;
using std::endl;
using std::fixed;

#include <vector>
using std::vector;

#include <iomanip>
using std::setprecision;

#include <typeinfo>

#include "employee.h"
using GTUEmployee::Employee;
#include "hourlyEmployee.h"
using GTUEmployee::HourlyEmployee;
#include "salariedEmployee.h"
using GTUEmployee::SalariedEmployee;
#include "commissionEmployee.h"
using GTUEmployee::CommissionEmployee;
#include "basePlusCommissionEmployee.h"
using GTUEmployee::BasePlusCommissionEmployee;

void printViaReference (const Employee & e);
void printViaPointer (const Employee * e);

int main () {
     // set floating-point output formatting
     cout << fixed << setprecision(2);

     vector<Employee *> employees(4);
     employees[0] = new SalariedEmployee("John", "Smith", "111-11-1111", 800);
     employees[1] = new HourlyEmployee("Karen", "Price", "222-22-2222", 16.75, 40);
     employees[2] = new CommissionEmployee("Sue", "Jones", "333-33-3333", 10000, .06);
     employees[3] = new BasePlusCommissionEmployee("Bob", "Lewis", "444-44-4444", 5000, .04, 300);

     for (size_t i = 0; i < employees.size(); ++i) {
          employees[i]->print();
          cout << endl;

          // downcast pointer
          BasePlusCommissionEmployee * derivedPtr = 
               dynamic_cast<BasePlusCommissionEmployee *>(employees[i]);
          if (derivedPtr != nullptr) {
               double oldBaseSalary = derivedPtr->getBaseSalary();
               cout << "old base salary: $" << oldBaseSalary << endl;
               derivedPtr->setBaseSalary( 1.10 * oldBaseSalary );
               cout << "new base salary with 10% increase is: $" << derivedPtr->getBaseSalary() << endl;
          }
          cout << "earned $" << employees[i]->earnings() << "\n\n";
     }

     for (size_t i = 0; i < employees.size(); ++i) {
          // typeid::name
          cout << "deleting object of " << typeid(*employees[i]).name() << endl;

          delete employees[i];
     }

     cout << endl;
     return 0;
} 