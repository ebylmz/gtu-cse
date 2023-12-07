#include <iostream>
using namespace std;

int countOnesH (char * s, int n);
int countOnes (char * s);

bool oddNumberOfOnes (char * s);
bool evenNumberOfOnes(char * s);

int main () {
    char sequence[] = "10101011";
    if (evenNumberOfOnes(sequence))
        cout << "Even number of ones exist in sequence\n";
    else
        cout << "Odd number of ones exist in sequence\n";  
    
    cout << "Number of 1s in sequence: " << countOnes(sequence) << endl;
}

bool oddNumberOfOnes (char * s) {
    if (*s == '\0')
        return false;
    else if (*s == '1')
        return oddNumberOfOnes(s + 1);
    else
        return evenNumberOfOnes(s + 1);
}

bool evenNumberOfOnes (char * s) {
    if (*s == '\0')
        return true;
    else if (*s == '1')
        return oddNumberOfOnes(s + 1);
    else
        return evenNumberOfOnes(s + 1);
}

int countOnesH (char * s, int n) {
    if (*s == '\0')
        return n;
    else if (*s == '1') 
        ++n;
    return countOnesH(s + 1, n);
}

int countOnes (char * s) {
    return countOnesH(s, 0);
}