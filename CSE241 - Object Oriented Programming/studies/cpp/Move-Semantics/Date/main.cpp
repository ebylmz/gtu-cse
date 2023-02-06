#include <iostream>
#include "date.h"

using std::cout;
using std::endl;

int main (void) {
    Date d(2001, 8, 25);

    // set the date format and print
    d.hu();
    cout << d << endl;
    d.us();
    cout << d << endl;
}