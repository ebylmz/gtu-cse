/* Operator Overloading: Overloading as Friend (Friend Functions & Friend Classes) */

/** baseAmount + 25
 *  it first checks to see if the operator + has been overloaded for the combination of a value
 *  of type Money and an integer. Since there is no such overloading, the system next looks
 *  to see if there is a constructor that takes a single argument that is an integer. If it finds a
 *  constructor that takes a single integer argument, it uses that constructor to convert the
 *  integer 25 to a value of type Money. The one-argument constructor says that 25 should
 *  be converted to an object of type Money whose member variable dollars is equal to
 *  25 and whose member variable cents is equal to 0. In other words, the constructor
 *  converts 25 to an object of type Money that represents $25.00. 
 */

/**  Overloading an operator as a nonmember gives you automatic type conversion
 *   of all arguments. Overloading an operator as a member gives you the efficiency of
 *   bypassing accessor and mutator functions and directly accessing member variables.
 *   There is a way to overload an operator (and certain functions) that offers both of
 *   these advantages. It is called overloading as a friend function and is our next topic. 
    
 *   Overloading the + operator as a friend will allow us to both directly access
 *   member variables and have automatic type conversion for all operands.
 */

/** Friend function
 *  A friend is not a member function; rather, it really is an ordinary function
 *  with extraordinary access to the data members of the class

 *  Friendship is granted, not taken

 *  We do not like friendship that much in OOP. Do not use friends function other than operation overload 
 *  The most common kinds of friend functions are overloaded operators. However,
    any kind of function can be made a friend function
 */

/* Friend class */
class F; // Forward declaration

class C {
    public:
        friend class F;
        // some definitions ....
};

class F {
    // some definitions ....
};

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

        /* There is no const keyword at the end of the fuction decleration
           because friends functions are not member of that class */

        /* friends keyword means: this global  function is my friend and can access all my data (private) */
        friend const Money operator+ (const Money & amount1, const Money & amount2);
        friend const Money operator- (const Money & amount1, const Money & amount2);
        friend bool operator== (const Money & amount1, const Money & amount2);
        friend bool operator!= (const Money & amount1, const Money & amount2);
        friend const Money operator- (const Money & amount);

    private:
        /* Take total amount as double and parse it to dollars and cents as int variables */
        int dollars;
        int cents;
        int dollarsPart (double amount) const;  
        int centsPart (double amount) const;
        int round (double number) const;
};

int main (void) {
    Money amount(10, 9);
    
    cout << "Your amount is ";
    amount.output();
    cout << endl;

    
    /**
     * nonmember-nonfriend: Global function, only access the class private data with accessor and mutator
     * 
     * member: Can access all the data like member function does. 
     * Not capable of automatic conversion both direction, only for arguments (caller.operator(arguments))  
     * 
     * friend: Like mixing of member and non-member function, takes two argument for binary operators and 
     *         can access all the data like member function
     * 
     * Automatic type conversion (produced by constructor)
     * member:                         amount.operator+(25);
     * nonmember-nonfriend & friend:   operator+(amount, 25);
     */

    amount = amount + 25;
    // works all the overload functions (member, non-member, friend)
    amount = 25 + amount;   
    // works only member and friend functions, function caller cannot be automatic converted   
    // 25.operator+(amount); // Invalid

    cout << "Added 50 in your account\n"
         << "Your new amount is ";
    amount.output();
    cout << endl;

    return 0;
}

/* Use const quantifier to prevent these kind of stuff: (m1 + m2).input() */
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
    int allCents1 = amount1.getDollars() * 100 + amount1.getCents();
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


bool operator== (const Money & amount1, const Money & amount2) {
    return amount1.getDollars() == amount2.getDollars() && amount1.getCents()  == amount2.getCents();
}

bool operator!= (const Money & amount1, const Money & amount2) {
    //! By reference we can reach the private data
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

Money::Money (int theDollars) : dollars(theDollars), cents(0) 
{/* Body intentionally empty. */}

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