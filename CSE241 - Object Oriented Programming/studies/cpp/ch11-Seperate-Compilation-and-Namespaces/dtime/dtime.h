// This is the header file dtime.h. This is the interface for the class
// DigitalTime. Values of this type are times of day. The values are
// input and output in 24-hour notation, as in 9:30 for 9:30 AM and 14:45 for 2:45 PM.

#ifndef DTIME_H
#define DTIME_H

#include <iostream>

using std::istream;
using std::ostream;

namespace DTimeEmirkan {
    class DigitalTime {
    public:
        DigitalTime(int theHour = 0, int theMinute = 0);
        
        int getHour() const;
        int getMinute() const;

        void advance(int minutesAdded);
        void advance(int hoursAdded, int minutesAdded);

        friend bool operator==(const DigitalTime & t1, const DigitalTime & t2);
        
        friend istream & operator>>(istream & ins, DigitalTime & t);
        friend ostream & operator<<(ostream & os, const DigitalTime & t);
    private:
        int hour;
        int minute;
    };
}

#endif