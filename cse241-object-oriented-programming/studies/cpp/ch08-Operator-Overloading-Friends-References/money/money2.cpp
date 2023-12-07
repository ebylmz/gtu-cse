/* Operator Overloading: Overloading as Member Functions */

/* When an operator is overloaded as a member of a class, 
   the first operand is the calling object.              */

/*
■ When overloading an operator, at least one parameter (one operand) of the resulting
overloaded operator must be of a class type.

■ Most operators can be overloaded as a member of the class, a friend of the class, or a
nonmember, nonfriend.

■ The following operators can only be overloaded as (nonstatic) members of the class: 
=, [], ->, and ().

■ You cannot create a new operator. All you can do is overload existing operators such as
+, -, *, /, %, and so forth.

■ You cannot change the number of arguments that an operator takes. For example, you
cannot change % from a binary to a unary operator when you overload %; you cannot
change ++ from a unary to a binary operator when you overload it.

■ You cannot change the precedence of an operator. An overloaded operator has the
same precedence as the ordinary version of the operator. For example, x*y + z
always means (x*y) + z, even if x, y, and z are objects and the operators + and *
have been overloaded for the appropriate classes.

■ The following operators cannot be overloaded: the dot operator (.), the scope resolution
operator (::), sizeof, ?:, and the operator .*, which is not discussed in this book.

■ An overloaded operator cannot have default arguments. */

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

        const Money operator+ (const Money & amount2) const;
        const Money operator- (const Money & amount2) const;
        bool operator== (const Money & amount2) const;
        bool operator!= (const Money & amount2) const;
        const Money operator- () const;
    private:
        /* Take total amount as double and parse it to dollars and cents as int variables */
        int dollars;
        int cents;
        int dollarsPart (double amount) const;  
        int centsPart (double amount) const;
        int round (double number) const;
};

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


/* Use const quantifier to prevent these kind of stuff: (m1 + m2).input() */
const Money Money::operator+ (const Money & amount2) const {
int allCents1 = dollars * 100 + cents;
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

const Money Money::operator- (const Money & amount2) const {
    int allCents1 = dollars * 100 + cents;
    int allCents2 = amount2.getDollars() * 100 + amount2.getCents();
    int diffAllCents = allCents1 - allCents2;
    int absAllCents = abs(diffAllCents);
    int finalDolars = absAllCents / 100;    
    int finalCents = absAllCents % 100;

    if (diffAllCents < 0) {
        finalDolars = -finalDolars;
        finalCents = -finalCents;
    }

    return Money(finalDolars, finalCents);
}


bool Money::operator== (const Money & amount2) const {
    return dollars == amount2.getDollars() && cents  == amount2.getCents();
}

bool Money::operator!= (const Money & amount2) const {
    //! By reference we can reach the private data
    return dollars != amount2.dollars || cents != amount2.cents;
}

const Money Money::operator- () const {
    return Money(-dollars, -cents);
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