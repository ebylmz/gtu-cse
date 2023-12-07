#include <iostream>
#include <fstream>

using namespace std;

class DayInYear {
public:
    DayInYear (int setMonth, int setDay);
    DayInYear (int setMonth);
    DayInYear ();

    // In C++, if a class has a constructor which can be called with a single argument, 
    // then this constructor becomes conversion constructor because such a constructor 
    // allows automatic conversion to the class being constructed. 

    int setDate (int monthValue, int dayValue);
    int setDay (int dayValue);
    int setMonth (int monthValue);

    void getDate (int & monthValue, int & dayValue) const; 
    int getDay() const; 
    int getMonth() const; 

    // Don't forget in operator overloading, there must be at least one Class type parameter
    friend istream & operator>> (istream & inStream, DayInYear & date);
    friend ostream & operator<< (ostream & outStream, const DayInYear & date);
    bool operator== (const DayInYear & other) const;
    //! use operator== if conversion constructor exist 
    bool operator!= (const DayInYear & other) const;

private:
    int day;
    int month;
    int checkDate ();
};

int main (void) {
    DayInYear today, birthday;
    
    cout << "Enter today's date (DD.MM): ";
    cin >> today;
    cout << "Enter your birthday (DD.MM): ";
    cin >> birthday;

    cout << "Today is " << today << endl;
    cout << "Your birthday is " << birthday << endl;

    if (today == birthday)
        cout << "Happy birthday to youu\n";

}

DayInYear::DayInYear (int monthValue, int dayValue) {
    if (setDate(monthValue, dayValue) != 0)
        exit(1);
}

DayInYear::DayInYear (int monthValue) {
    if (setMonth(monthValue) != 0)
        exit(1);
}

DayInYear::DayInYear () : month(1), day(1) {/* Empty */}

int DayInYear::setDate (int monthValue, int dayValue) {
    int r;
    if (setMonth(monthValue) == 0)
        r = setDay(dayValue);
    else
        r = 1;
    return r;
}

int DayInYear::setDay (int dayValue) {
    int r;

    if (1 <= dayValue && dayValue <= 31) {
        day = dayValue;
        r = 0;
    }
    else {
        cerr << "Invalid day value\n";
        r = 1;
    }
    return r;
}

int DayInYear::setMonth (int monthValue) {
    int r;

    if (1 <= monthValue && monthValue <= 12) {
        month = monthValue;
        r = 0;
    }
    else {
        cerr << "Invalid month value\n";
        r = 1;
    }
    return r;
}

void DayInYear::getDate (int & monthValue, int & dayValue) const {
    monthValue = getMonth();
    dayValue = getDay();
}

inline int DayInYear::getDay() const { return day; }

inline int  DayInYear::getMonth() const { return month; }

istream & operator>> (istream & inStream, DayInYear & date) {
    char dot;
    int dayValue, monthValue;

    inStream >> monthValue >> dot >> dayValue;

    if (dot == '.')
        date.setDate(monthValue, dayValue);
    else
        cerr << "Invalid date format. Proper format is MM.DD\n";
    return inStream;
}

ostream & operator<< (ostream & outStream, const DayInYear & date) {
    outStream << date.getMonth() << '.' << date.getDay();
    return outStream;
}

bool DayInYear::operator== (const DayInYear & other) const {
    return  getDay() == other.getDay() &&
            getMonth() == other.getMonth();
} 

bool DayInYear::operator!= (const DayInYear & other) const {
    return ! (*this == other);
}