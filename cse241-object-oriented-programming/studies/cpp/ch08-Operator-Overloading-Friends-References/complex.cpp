#include <iostream>
#include <fstream>

using namespace std;

class Complex {
public:
    Complex (int realPart, int imagPart);
    Complex (int realPart);
    Complex ();

    void setNumber (int realPart, int imagPart);
    void setRealPart (int realPart);
    void setImagPart (int imagPart);

    void getNumber (int & realPart, int & imagPart) const;
    int getRealPart () const;
    int getImagPart () const;

    friend istream & operator>> (istream & inStream, Complex & c);
    friend ostream & operator<< (ostream & outStream, const Complex & c);
    friend const Complex operator+ (const Complex & c1, const Complex & c2);
    friend const Complex operator- (const Complex & c1, const Complex & c2);
    const Complex operator- () const;
    friend const Complex operator* (const Complex & c1, const Complex & c2);
    friend const Complex operator/ (const Complex & c1, const Complex & c2);
    
    const Complex operator++ ();
    const Complex operator++ (int);

private:
    int real;
    int imag;
};

int main (void) {  
    Complex c1, c2;
    cout << "Complex1: ";
    cin  >> c1;   

    cout << "Complex1: ";
    cin  >> c2;   

    cout << '(' << c1 << ')' << " + " << '(' <<  c2 << ')' << " = " << c1 + c2 << endl;
    cout << '(' << c1 << ')' << " - " << '(' <<  c2 << ')' << " = " << c1 - c2 << endl;
    cout << '(' << c1 << ')' << " * " << '(' <<  c2 << ')' << " = " << c1 * c2 << endl;
    // cout << '(' << c1 << ')' << " / " << '(' <<  c2 << ')' << " = " << c1 / c2 << endl;
}

Complex::Complex (int realPart, int imagPart) : real(realPart), imag(imagPart) {}

Complex::Complex (int realPart) : real(realPart), imag(0) {}

Complex::Complex () : real(0), imag(0) {}

void Complex::setNumber (int realPart, int imagPart) {
    real = realPart;
    imag = imagPart;
}

inline void Complex::setRealPart (int realPart) { real = realPart; }

inline void Complex::setImagPart (int imagPart) { imag = imagPart; }

void Complex::getNumber (int & realPart, int & imagPart) const {
    realPart = real;
    imagPart = imag;
}

inline int Complex::getRealPart () const { return real; }

inline int Complex::getImagPart () const { return imag; }

istream & operator>> (istream & inStream, Complex & c) {
    int realPart, imagPart;
    char sign, i;
    
    inStream >> realPart >> sign >> imagPart >> i;
    if ((sign != '+' && sign != '-' ) || i != 'i')
        cerr << "Invalid format (RR+IIi)\n";
    else {
        if (sign == '-')
            imagPart = -imagPart;
        c.setNumber(realPart, imagPart);
    }

    return inStream;
}

ostream & operator<< (ostream & outStream, const Complex & c) {
    if (c.getRealPart() != 0)
        outStream << c.getRealPart();
    
    if (c.getImagPart() != 0) {
        if (c.getImagPart() > 0)
            cout << '+'; 
        cout << c.getImagPart() << 'i';
    }

    return outStream;
}

const Complex operator+ (const Complex & c1, const Complex & c2) {
    int realPart = c1.getRealPart() + c2.getRealPart();
    int imagPart = c1.getImagPart() + c2.getImagPart();
    return Complex(realPart, imagPart);
}

const Complex operator- (const Complex & c1, const Complex & c2) {
    return c1 + -c2;
}

const Complex Complex::operator- () const {
    return Complex(-getRealPart(), -getImagPart());
}

const Complex operator* (const Complex & c1, const Complex & c2) {
    // i * i = -1
    int realPart = c1.getRealPart() * c2.getRealPart() - (c1.getImagPart() * c2.getImagPart());
    int imagPart = c1.getRealPart() * c2.getImagPart() + (c1.getImagPart() * c2.getRealPart());
    return Complex(realPart, imagPart);
}

const Complex operator/ (const Complex & c1, const Complex & c2) {
    // NOT IMPLEMENTED YET
    // https://www.youtube.com/watch?v=03kgmv50gBM
}


const Complex Complex::operator++ () {
    return Complex(++real, ++imag);
}

const Complex Complex::operator++ (int) {
    return Complex(real++, imag++);
}
