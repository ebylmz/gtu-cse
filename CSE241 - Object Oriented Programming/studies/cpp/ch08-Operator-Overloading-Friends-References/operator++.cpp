#include <iostream>

using namespace std;

class T {
    public:
        T() : value(0) {}
        T(int v) : value(v) {}

        T operator++ (int); 
        friend ostream & operator<< (ostream & outSteam, const class T & classT);
        friend istream & operator>> (istream & isStream, class T & classT);
    private:
        int value;
};

int main (void) {
    T a;

    /* If operator++ overloaded as const T operator++ (int)
       then (a++)++  cause an error becasue a++ return const a value */
    cout << (a++)++ << endl;  
    cout << a << endl;  
}

T T::operator++ (int) {
    T tmp(*this);
    ++value;
    return tmp;
}

ostream & operator<< (ostream & outStream, const T & classT) {
    outStream << classT.value;
    return outStream;
}

istream & operator>> (istream & inStream, T & classT) {
    inStream >> classT.value;
    return inStream;
}