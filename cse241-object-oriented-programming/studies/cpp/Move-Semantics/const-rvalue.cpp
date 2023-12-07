#include <iostream>
#include <string>

using std::string;
using std::cout;
using std::endl;


void printName (string & s);           // lvalue referance
void printName (const string & s);     // const lvalue referance
void printName (string && s);          // ravlue referance

void printAge (const double & s);

int main () {
    string firstName = "Emirkan";
    string lastName = "Yilmaz";
    string name = firstName + lastName; 
    
    
    // constant lvalue reference works!
    // something like this happen on underneath the hood
    // string tmp = "Arda";
    // const string & anothername = tmp;
    const string & anothername = "Arda";    
    
    printName(name);               
    printName(anothername);
    printName(firstName + lastName);

    double age1 = 17;
    printAge(age1);  
    printAge(17);   // int literal matchs const lvalue reference
}

void printName (string & s) {
    cout << "[lvalue] " << s << endl;
}

void printName (const string & s) {
    cout << "[const lvalue] " << s << endl;
}

void printName (string && s) {
    cout << "[rvalue] " << s << endl;
}

void printAge (const double & age) {
    cout << "[conts lvalue] " << age << endl;
}