// This is cse241 final exam question askey by YSA
// Reverse the given integer recursively without tail recursive

#include <iostream>

// Helper function for reversing an integer
int reverseIntBase (int n, int & base) {
    if (n < 10)
        return n;
    else {
        int preResult = reverseIntBase(n / 10, base);
        base *= 10; // new result will be has 1 more digit

        return (n % 10) * base + preResult;   
    }
}

int reverseInt (int n) {
    int base = 1;   // power of 10
    return reverseIntBase(n, base);
}

int main (void) {
    int n;
    while (1) {
        std::cout << "Enter an integer: ";
        std::cin >> n;
        std::cout << "Reverse: " << reverseInt(n) << "\n\n"; 
    }
}