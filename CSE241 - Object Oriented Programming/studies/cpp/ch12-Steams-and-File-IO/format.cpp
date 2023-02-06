#include <iostream>
#include <fstream>

using namespace std;

/*
cout.setf(ios::showpoint);
cout.precision(2);
cout.setf(ios::fixed);
*/

int main (void) {
    ifstream inStream("person.txt");
    
    if (! inStream.fail()) {
        string next;

        // Reads line by line
        while (getline(inStream, next))
            cout << next << endl;

        // Reads word by word
        /*
        while (inStream >> next)
            cout << next << endl;
        */
        inStream.close();
    }

    
    // ofstream outStream("person.txt");

}