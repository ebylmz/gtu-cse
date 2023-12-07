// Demonstrating exception rethrowing.
#include <iostream>
#include <exception>

using std::cout;
using std::endl;
using std::exception;

void throwException() throw (exception) {
	try {
		cout << "Function throwException throws an exception\n";
		throw exception(); 
	} 
	catch ( exception & )
	{
		cout << "Exception handled in function throwException"
		<< "\n Function throwException rethrows exception";
		// this exception is so hard to handle for me
        // rethrow exception for further processing
        throw; 
	} 
	cout << "This also should not print\n";
} 

int main() {
	try {
		cout << "\nmain invokes function throwException\n";
		throwException();
		cout << "This should not print\n";
	} 
	catch ( exception & ) {  
		cout << "\n\nException handled in main\n";
	} 
	cout << "Program control continues after catch in main\n";
	return 0;
} 