#include <iostream>
#include <string>

using namespace std;

class DayInWeek {
    public:
        // enum class Day {monday, tuesday, wednesday, thursday, friday, saturday, sunday};

        DayInWeek (int dayValue);
        DayInWeek ();

        int getDay () const;
        
        friend ostream & operator<< (ostream & outStream, const DayInWeek & day);
        friend istream & operator>> (istream & inStream, DayInWeek & day);
        friend const DayInWeek operator+ (const DayInWeek & day, int n);          
        friend const DayInWeek operator+ (const DayInWeek & day1, const DayInWeek & day2);          
        
        /* Why these are member rather than friend?
           Because there is no advantage of using friend.
           Remember we use friend to reach class data and take advantage of automatic
           conversion. Since ++ is unary operator there is only one or no argument. */
        DayInWeek operator++ ();          // prefix   
        DayInWeek operator++ (int);       // postfix 

        /* Prefix and Postfix notation returs value not reference.
           Becasue in postfix notation function had to return
           local temporary variable which die very soon */

        /* Add a second parameter of type int. This is just a marker for the compiler.
           In that way these two function signatures become different. 
           You do not give a second int argument when you invoke x++ or x-- */
    private:
        void testDay ();
        int dayValue;
};

int main (void) {
    DayInWeek today(2);
    DayInWeek tomarrow = today + 1;
    cout << "Today is " << today << endl;
    cout << "Tomarrow is " << tomarrow << endl;
    cout << "Today + Tomarrow is " << today + tomarrow << endl;
}

const DayInWeek operator+ (const DayInWeek & day, int n) {
    //! NOT IMPLEMENTED YET 
    return DayInWeek((day.getDay() + n) % 7);
}

const DayInWeek operator+ (const DayInWeek & day1, const DayInWeek & day2) {
    //! NOT IMPLEMENTED YET 
    return DayInWeek((day1.dayValue + day2.dayValue) % 7); 
}   

// Prefix  ++c
DayInWeek DayInWeek::operator++ () {
    ++dayValue;
    return *this;
}

// Postfix c++
DayInWeek DayInWeek::operator++ (int) {
    DayInWeek tmpDay(*this); // copy constructor
    
    dayValue = dayValue == 7 ? 1 : ++dayValue;
    return tmpDay; 
}   

ostream & operator<< (ostream & outStream, const DayInWeek & day) {
    switch (day.getDay()) {
        case 1:
            outStream << "Monday";  break;
        case 2:
            outStream << "Tuesday"; break;
        case 3:
            outStream << "Wednesday";   break;
        case 4:
            outStream << "Thursday";    break;
        case 5:
            outStream << "Friday";  break;
        case 6:
            outStream << "Saturday";    break;
        case 7:
            outStream << "Sunday";  break;
    }
    return outStream;
}

istream & operator>> (istream & inStream, DayInWeek & day) {
    inStream >> day.dayValue;
    day.testDay();
    return inStream;
}

DayInWeek::DayInWeek (int dayValue) : dayValue(dayValue) {
    testDay();    
}

DayInWeek::DayInWeek () : dayValue(1) {}

inline int DayInWeek::getDay () const {
    return dayValue; 
}

void DayInWeek::testDay () {
    if (dayValue < 1 || dayValue > 7) {
        cerr << "Invalid day value: " << dayValue << endl;
        exit(1);
    }
}
