/* Overloading operator[] return value as r-value and l-value */

#include <iostream>
#include <cstdlib>

using namespace std;

class CharPair
{
    public:
        CharPair( ){ /*Body intentionally empty*/ }
        CharPair(char firstValue, char secondValue) : first(firstValue), second(secondValue) { /*Body intentionally empty*/}
        
        /* Their signatures are not same because of the at the end of the const of r-value overloading
           Compiler smart enough to distinguish between these two overloading */
        const char& operator[] (int index) const;    // For r-value
        char& operator[] (int index);                // For l-value

    private:
        char first;
        char second;
};
int main (void) {
    CharPair a('A', 'B');
    CharPair b(a);  // copy constructor
    const CharPair c('C', 'D');  
    
    cout  << a[1] << endl; 
    cout  << b[2] << endl; 
    cout  << c[1] << endl; 
    // if l-value operator[] overloading does not exist then b[] is cause an error
    return 0;
}

const char& CharPair:: operator[](int index) const {
    cout << "const overloading\n";
    if (index == 1)
        return first;
    else if (index == 2)
        return second;
    else {
        cout << "Illegal index value.\n";
        exit(1);
    }
}

char& CharPair:: operator[](int index) {
    cout << "non const overloading\n";
    if (index == 1)
        return first;
    else if (index == 2)
        return second;
    else {
        cout << "Illegal index value.\n";
        exit(1);
    }
}