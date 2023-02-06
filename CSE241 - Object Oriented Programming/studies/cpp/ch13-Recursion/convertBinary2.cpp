#include <iostream>
#include <cstdlib>

double pow_h (double curVal, int base, int exp) {
        if (exp > 0)
            return pow_h(curVal * base, base, exp - 1);
        else if (exp < 0)
            return pow_h(curVal / base, base, exp + 1);
        else 
            return curVal; 
}

double pow (double base, int exp) {
    return pow_h(1, base, exp);
} 

char * binary (int decimal) {
    // first find the digit number of decimal
    int n = 0;

    for (int i = 1; i <= decimal; i *= 2)
        ++n;
    
    char * binary = new char[n + 1];
    for (int i = n - 1; i >= 0; --i) {
        int val = pow(2, i);
        if (decimal >= val) {
            binary[(n - 1) - i] = '1';
            decimal -= val;
        }
        else
            binary[(n - 1) - i] = '0';
    }
    // binary[n] = '\0';    // new starts each cell from ASCII Value 0 which is '\0'
    return binary;
}

int main (int argc, char * args[]) {
    // std::cout << pow(2, -3) << std::endl; // 0.125
    for (int i = 1; i < argc; ++i) {
        int decimal = atoi(args[i]); 
        std::cout << decimal << " - " << binary(decimal) << std::endl;
    }
}