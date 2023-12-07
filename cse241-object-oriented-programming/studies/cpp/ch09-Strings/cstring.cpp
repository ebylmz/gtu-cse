#include <iostream>
#include <string>

using namespace std;

// strings still contain deliminater '\0'
void test1 () {
    string s("Hello World!");

    for (int i = 0; i < s[i] != '\0'; ++i)
        cout << s[i];
    cout << endl;

    for (int i = 0; i < s.length() + 1; ++i)
        cout << s[i] << " : " << static_cast<int>(s[i]) << endl;
}

void test2 () {
    // char s[5] = "Hello";    // This line does not compile 
}

void test3 () {
    const int LINE_SIZE = 80;
    char line[LINE_SIZE];

    char preChar = ' ', nextChar;
    cout << "Sentence: ";
    do {
        cin.get(nextChar);
        if (preChar == ' ')
            cin.putback(toupper(nextChar));
        preChar = nextChar;
        cout << nextChar;        
    } while (nextChar != '\n');

    // Other usefull functions
    // cin.getline(line, LINE_SIZE);
    // cin.ignore(1000, '\n');
}

void test4 () {
    while (1) {
        char c;
        cin.get(c);
        if (c == '\n') 
            break;
        else 
            cin.putback(c);
    }
}

int main (void) {
    test3();
}