# Library
* You compile all the classes into the same binary file, you sent the binary
file with the header file. The costumer only see the header files and the implementation
comes from the library. So we never compile our library code 

# Compilation Unit
* A file, along with all files #included in file

* Every Compilation unit has unnamed namespace, with this all names are than local to compilation unit.
  (https://youtu.be/eu-FKkiNnTM?list=PLuid2q5iknODFPHJPoO0gE1aWL9LGnqZ1&t=2533)

* Use unnamed namespace to keep things "local". It's not possible to use an unnamed namespace outside of the compilation unit.

* Every time you make a compiler call, you are creating a new compilation unit

# Namespace
What is the purpose of using namespace?

* All code goes in some namespace. Unless specified all the definition in global namespace. For example main() function is in global namespace.

* No need for using directive (implied "automatic" using directive)

* Global namespace is always available

* The global namespace does not have
a using directive because you are always using the global namespace.

* Namespaces can be defined seperate files for exp I define part of my namespace in interface file (header) and I define part of it in implementation file.

* Namespaces should be unique like your last name, institiuon name... (DayOfYearEBY)

* Namespace groping is parting the namespace and compining in compilation unit
for exp. header file contains the decleration and implementation file contains definitions both of are under the same namespace   

* using decleration
    using std::cout; // like variable decleration
 using directive
    using namespace std;
 without using directve:
    NS1::fun1();


* Names in the global namespace and names in the unnamed namespace may both be
accessed without a qualifier. However, names in the global namespace have global
scope (all the program files), whereas names in an unnamed namespace are local to a
compilation unit.

* Main function must be in global namespace. Otherwise program will be compile but not linked. Because main function needed and linker calls just main not like NS1::main.    

* There are two good ways to hide a helping function for a class. You can make the
function a private member function of the class or you can place the helping function
in the unnamed namespace for the implementation file of the class. If the function
naturally takes a calling object, then it should be made a private member function. If
it does not naturally take a calling object, you can make it a static member function
(for example, DigitalTime::readHour in Displays 11.1 and 11.2 ) or you can place
it in the unnamed namespace of the implementation file (for example, readHour in
Displays 11.8 and 11.9 .)

* We use unnamed namespace to keep the names local in current compilation unit, by doing this the related functions only available in current compilation unit. Outside of the compilation unit these definitions wont be available. So we can use same name in different compilation unit. Like global function but not exactly. Encapsulation principle is applied.

* The number of unnamed namepace is same as the file which will be compiled or the number of compilation unit.

* Don't put unnamed namespace in header file (*.h), because when *.h included in *.cpp the code you put in *.h in unnmaed namesapce will become part of unnamed namepspace (*.cpp unnamed namespace extented) of the *.cpp which includes *.h