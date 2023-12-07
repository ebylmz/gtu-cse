// This is cse241 final exam question askey by YSA
// Convert the given integer recursively 

#include <iostream>

long convertBinary (int n) {
    // decimal and binary values are same for 1 and 0  
    if (n < 2)
        return n;
    else 
        // first remainder become last digit in binary equavelent
        return convertBinary(n / 2) * 10 + n % 2; 
}


int main (void) {
    int n;
    while (1) {
        std::cout << "Enter an integer: ";
        std::cin >> n;
        std::cout << "Binary: " << convertBinary(n) << "\n\n"; 
    }
}