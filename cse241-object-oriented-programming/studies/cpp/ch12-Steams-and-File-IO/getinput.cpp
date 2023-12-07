#include <iostream>
#include <string>
#include <limits>

using namespace std;

void getInput (int & input, const string prompt);

int main (void) {
    int n;
    getInput(n, "Enter an int value: ");
    cout << "Entered: " << n << endl;
}

void getInput (int & input, const string prompt) {
    cout << prompt;
    cin  >> input;

    while (cin.fail()) {
        cin.clear();
        cin.ignore(std::numeric_limits<std::streamsize>::max(), '\n');
        /* The cin.clear() clears the error flag on cin (so that future I/O operations will work correctly),
         and then cin.ignore(10000, '\n') skips to the next newline (to ignore anything else on the same line
        as the non-number so that it does not cause another parse failure). It will only skip up to 10000 characters, 
        so the code is assuming the user will not put in a very long, invalid line. */
    
        cout << "(!) Invalid input\n"
             << prompt;
        cin  >> input;
    }
}

/*
You enter the
if (!(cin >> input_var))
statement if an error occurs when taking the input from cin. If an error occurs then an error flag is set and future attempts to get input will fail. That's why you need
cin.clear();
to get rid of the error flag. Also, the input which failed will be sitting in what I assume is some sort of buffer. 
When you try to get input again, it will read the same input in the buffer and it will fail again. That's why you need
cin.ignore(10000,'\n');
It takes out 10000 characters from the buffer but stops if it encounters a newline (\n). The 10000 is just a generic large value.
*/