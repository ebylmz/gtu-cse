// https://www.researchgate.net/post/What_is_the_role_of_static_function_and_this_pointer_in_C

/*  A static function is a member function of a class that can be called even when
    an object of the class is not initialized. A static function cannot access any 
    variable of its class except for static variables. The 'this' pointer points to
    the object that invokes the function. A regular function can access static 
    variables but a static function cannot access regular variables     */

#include <iostream>

using namespace std;

// If I decleared as global then it can be used at everwhere
// static int n = 0;

int fun () {
    // But if I decleared localy static variable can only access in that  scope
    static int n;
    return ++n;
}


class Container {
    public:
        Container(char containerName = 'C');
        Container();
        static int getContainerNumber ();
    private:
        // error: ISO C++ forbids in-class initialization of non-const static member
        static int containerNumber;
        char name;
};

// If I did not do decleration here then error occurs
// undefined reference to `Container::containerNumber'
int Container::containerNumber = 0;

int main (void) {
    /*
    cout << fun() << endl;
    cout << fun() << endl;
    cout << fun() << endl;
    cout << fun() << endl;
    */
   cout << "Initial container number: " << Container::getContainerNumber() << endl;
   Container c1('A'), c2('B'), c3('Z');
}

Container::Container (char containerName) : name(containerName) {
    ++containerNumber;
    cout << "Container " << containerName << " created\n"
         << "There are " << Container::getContainerNumber() << " container exist\n";
}

inline int Container::getContainerNumber () {
    return containerNumber;
}