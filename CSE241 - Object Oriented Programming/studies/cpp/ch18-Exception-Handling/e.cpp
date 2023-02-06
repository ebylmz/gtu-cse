// Illustriates what happens if throwing exception does not catch 
//  If you didn't have a suitable handler the C++ runtime would call 
// unexpected() built-in function that would call abort() and terminate the program.
#include <iostream>

using std::cin;
using std::cout;
using std::endl;

class DividedByZero {};
class SomeException {};

double division (int numerator, int denominator) throw (DividedByZero) {
	if (denominator == 0)
		throw DividedByZero();
	return numerator / static_cast<double>(denominator);
}

int main () {
	int numerator, denominator;
	cout << "numerator: ";
	cin >> numerator; 
	cout << "denominator: ";
	cin >> denominator;
	try {
		double result = division(numerator, denominator);
		cout << numerator << "/" << denominator << " = " << result << endl; 
	}
	// catch (DividedByZero) {
	// 	cout << "Divided by zero error\n";
	// }
	catch (SomeException) {
		cout << "Something bad happen. Aborted.";
	}
}
