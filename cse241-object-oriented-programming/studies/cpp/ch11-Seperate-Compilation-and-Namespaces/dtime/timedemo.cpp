// This is the application file timedemo.cpp. This program
// demonstrates hiding the helping functions in an unnamed namespace.

#include <iostream>
#include "dtime.h"

void readHour( int& theHour);

int main() {
     using std::cout;
     using std::cin;
     using std::endl;
     using DTimeEmirkan::DigitalTime;

     // int theHour;
     // readHour(theHour);  // comes from global namespace
    
     DigitalTime clock, oldClock;
     cout << "Enter the time in 24-hour notation: ";
     cin  >> clock;
     oldClock = clock;   // shallow copy
     
     clock.advance(15);
     
     if (clock == oldClock)
          cout << "Something is wrong." << "You entered " << oldClock << endl;
     else {
          cout << "15 minutes later the time will be "<< clock << endl;
          clock.advance(2, 15);
          cout << "2 hours and 15 minutes after that\n" 
               << "the time will be " << clock << endl;
     }
     
     return 0;
}

void readHour( int& theHour) {
     using std::cout;
     using std::cin;

     cout << "Let's play a time game.\n"
     << "Let's pretend the hour has just changed.\n"
     << "You may write midnight as either 0 or 24,\n"
     << "but, I will always write it as 0.\n"
     << "Enter the hour as a number (0 to 24): ";
     cin >> theHour;
}