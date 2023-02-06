#include <iostream>
#include <cctype>
#include "dtime.h"

using std::ostream;
using std::istream;
using std::cerr;
using std::cout;
using std::cin;

namespace {
    int digitToInt (char c) { 
        return static_cast<int>(c - '0'); 
    }
    
    void readHour(int & theHour) {
        char c1, c2;
        cin >> c1 >> c2;
        
        if ( !( isdigit(c1) && (isdigit(c2) || c2 == ':' ) ) ) {
            cerr << "Error: illegal input to readHour\n";
            exit(1);
        }

        if (isdigit(c1) && c2 == ':')
            theHour = digitToInt(c1);
        else { //(isdigit(c1) && isdigit(c2)) 
            theHour = digitToInt(c1) * 10 + digitToInt(c2);
            cin >> c2; //discard ':'
            if (c2 != ':')
            {
            cerr << "Error: illegal input to readHour\n";
            exit(1);
            }
        }

        if (theHour == 24)
            theHour = 0; //Standardize midnight as 0:00
        if (theHour < 0 || theHour > 23) {
            cerr << "Error: illegal input to readHour\n";
            exit(1);
        }
    }   

    void readMinute(int & theMinute) {
        char c1, c2;
        cin >> c1 >> c2;
        if (!(isdigit(c1) && isdigit(c2))) {
            cerr << "Error: illegal input to readMinute\n";
            exit(1);
        }

        theMinute = digitToInt(c1)*10 + digitToInt(c2);
        if (theMinute < 0 || theMinute > 59) {
            cerr << "Error: illegal input to readMinute\n";
            exit(1);
        }
    }

} // unnamed namespace

namespace DTimeEmirkan {
    DigitalTime::DigitalTime(int theHour, int theMinute) 
    : hour (theHour), minute(theMinute) {
        if (theHour < 0 || 23 < theHour || theMinute < 0 || 59 < theMinute) {
            cerr << "Invalid time format\n";
            exit(1);
        }
    }

    inline int DigitalTime::getHour() const { return hour; }

    inline int DigitalTime::getMinute() const { return minute; }
    
    void DigitalTime::advance(int minutesAdded) {
        int grossMinutes = minute + minutesAdded;
        int hourAdjustment = grossMinutes / 60;

        minute = grossMinutes % 60;
        hour = (hour + hourAdjustment) % 24;
    }

    void DigitalTime::advance(int hoursAdded, int minutesAdded) {
        hour = (hour + hoursAdded) % 24;
        advance(minutesAdded);
    }

    bool operator==(const DigitalTime & t1, const DigitalTime & t2) {
        return t1.getHour() == t2.getHour() && t1.getMinute() == t2.getMinute();
    }

    istream & operator>>(istream & ins, DigitalTime & t) {
        // remember friend functions are not member of class but 
        // it can reach the private part of the class
        readHour(t.hour);
        readMinute(t.minute);
        return ins;
    }

    ostream & operator<<(ostream & outs, const DigitalTime & t) {
        if (t.getHour() < 10) 
            outs << "O";
        outs << t.getHour() << ":"; 

        if (t.getMinute() < 10)
            outs << "0";
        outs << t.getMinute();
            
        return outs;
    }
} //DTimeEmirkan


