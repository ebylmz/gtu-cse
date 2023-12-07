#include <iostream>
#include "..\exception.h"

using std::cout;
using std::cin;
using std::endl;
using ExceptionEBY::DividedByZero;

double quotient (int numerator, int denominator) throw (DividedByZero)  {
    if (denominator == 0)
        throw DividedByZero();

    return static_cast<double>(numerator) / denominator;
}

int main (void) {
    int num, denom;
    double result;

    cout << "\nEnter two integers (end-of-file to end): ";    
    while (cin >> num >> denom) {
        try {
            result = quotient(num, denom);
            cout << "The quotient is " << result << endl;
        }
        catch (DividedByZero & e) {
            cout << "Exception occured: " << e.what() << endl;
        }
        cout << "\nEnter two integers (end-of-file to end): ";
    }
    cout << endl;
    return 0;
}