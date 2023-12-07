/* Operator Overloading: Overloading Basics */

/* An operator is just a function that uses a different syntax for invocations */

/** A (binary) operator, such as +, -, /, %, and so forth, is simply a function that is called using
 *  a different syntax for listing its arguments. With a binary operator, the arguments are listed
 *  before and after the operator; with a function the arguments are listed in parentheses after
 *  the function name. An operator definition is written similar to a function definition, except
 *  that the operator definition includes the reserved word "operator" before the operator
 *  name. The predefined operators, such as +, -, and so forth, can be overloaded by giving
 *  them a new definition for a class type. 
 */

/** Use const quantifier to prevent these kind of stuff: (m1 + m2).input()
 
 *  To use nonmember overloaded operators, there exist accesser and mutator functions, 
 *  otherwise how we can reach the private data 
*/


#include <iostream>
#include <cstdlib>
#include <cmath>

using namespace std;

class Money {
    public:
        Money ();
        Money (double amount);
        Money (int theDollars, int theCents);
        Money (int theDollars);

        double getAmount () const;
        int getDollars () const;
        int getCents () const;
        void input (); 
        void output () const;

    private:
        /* Take total amount as double and parse it to dollars and cents as int variables */
        int dollars;
        int cents;
        int dollarsPart (double amount) const;  
        int centsPart (double amount) const;
        int round (double number) const;
};

/* Operator Overloading */
const Money operator+ (const Money & amount1, const Money & amount2);
const Money operator- (const Money & amount1, const Money & amount2);
bool operator== (const Money & amount1, const Money & amount2);
bool operator!= (const Money & amount1, const Money & amount2);
const Money operator- (const Money & amount);     // Unary -


int main (void) {
    Money yourAmount, myAmount(10, 9);
    cout << "Enter an amount of money: ";
    yourAmount.input( );
    
    cout << "Your amount is ";
    yourAmount.output( );
    cout << endl;
    
    cout << "My amount is ";
    myAmount.output( );
    cout << endl;
    
    if (yourAmount == myAmount)
        cout << "We have the same amounts.\n";
    else
        cout << "One of us is richer.\n";
    
    Money ourAmount = yourAmount + myAmount;
    yourAmount.output( ); cout << " + "; myAmount.output( );
    cout << " equals "; ourAmount.output(); cout << endl;
    
    Money diffAmount = yourAmount - myAmount;
    yourAmount.output( ); cout << " - "; myAmount.output( );
    cout << " equals "; diffAmount.output(); cout << endl;

    return 0;
}

const Money operator+ (const Money & amount1, const Money & amount2) {
    int allCents1 = amount1.getDollars() * 100 + amount1.getCents();
    int allCents2 = amount2.getDollars() * 100 + amount2.getCents();
    int sumAllCents = allCents1 + allCents2;
    int absAllCents = abs(sumAllCents);
    int finalDolars = absAllCents / 100;    
    int finalCents = absAllCents % 100;

    if (sumAllCents < 0) {
        finalDolars = -finalDolars;
        finalCents = -finalCents;
    }

    return Money(finalDolars, finalCents);
}

const Money operator- (const Money & amount1, const Money & amount2) {
    return amount1 + -amount2; // operator+(amount1, operator-(amount2));
}

bool operator== (const Money & amount1, const Money & amount2) {
    return amount1.getDollars() == amount2.getDollars() && amount1.getCents() == amount2.getCents();
}

bool operator!= (const Money & amount1, const Money & amount2) {
    return ! (amount1 == amount2);
}

const Money operator- (const Money & amount) {
    return Money(-amount.getDollars(), -amount.getCents());
}    

Money::Money () : dollars(0), cents(0)
{/* Body intentionally empty. */}

Money::Money (double amount) : dollars(dollarsPart(amount)), cents(centsPart(amount)) 
{/* Body intentionally empty. */}

Money::Money (int theDollars, int theCents) {
    if ((theDollars >= 0 && theCents >= 0) || (theDollars <= 0 && theCents <= 0)) {
        dollars = theDollars;
        cents = theCents;
    }
    else {
        cout << "Inconsistent money data.\n";
        exit(1);
    }
}

inline double Money::getAmount () const {
    return dollars + cents * 0.01;
}

inline int Money::getDollars () const {
    return dollars;
}

inline int Money::getCents () const {
    return cents;
}

void Money::input () {
    char dollarSign;
    cin >> dollarSign; //hopefully

    if (dollarSign != '$') {
        cout << "No dollar sign in Money input.\n";
        exit(1);
    }
    else {
        double amountAsDouble;
        cin >> amountAsDouble;
        dollars = dollarsPart(amountAsDouble);
        cents = centsPart(amountAsDouble);
    }
}

void Money::output () const {
    int absDollars = abs(dollars);
    int absCents = abs(cents);

    if (dollars < 0 || cents < 0)
        cout << "$-";
    else
        cout << '$';
    cout << absDollars;

    if (absCents >= 10)
        cout << '.' << absCents;
    else
        cout << '.' << '0' << absCents;
}

inline int Money::dollarsPart (double amount) const {
    return static_cast<int>(amount);
}

int Money::centsPart (double amount) const {
    double double_cents = amount * 100; 
    int int_cents = (round(fabs(double_cents))) % 100;

    if (amount < 0)
        int_cents = -int_cents;

    return int_cents;
}

inline int Money::round(double number) const {
    return static_cast<int>(floor(number + 0.5));
}