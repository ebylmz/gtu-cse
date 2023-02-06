/* An example code for static members and static functions */

#include <iostream>

using namespace std;

class Server {
    public:
        Server (char letterName);
        Server ();
    /*  A 'const member function' is not allowed to modify the object it is called on,
        but static member functions are not called on any object. It is used directly
        by scope resolution operator. Thus having a const static member function makes
        no sense, hence it is illegal.     */
        static int getTurn () const;
        static bool stillOpen () const;
        void serveOne ();
    private:
        static int turn;
        static bool nowOpen;
        static int lastServed;
        char name;
};

//! Initialization of static variables at global
int Server::turn = 0;
int Server::lastServed = 0;
bool Server::nowOpen = true;

int main (void) {
    Server s1('A'), s2('B');
    int number, count;

    do {
        cout << "How many in your group? ";
        cin >> number;

        if (number > 0) {
            cout << "Your turns are: ";
            for (count = 0; count < number; count++)
                cout << Server::getTurn( ) << ' ';
            cout << endl;
        }
    
        s1.serveOne( );
        s2.serveOne( );
    } while (Server::stillOpen( ));
    cout << "Now closing service.\n";
}

Server::Server (char letterName) : name(letterName) {
    char c = toupper(letterName);
    if (c < 'A'  && c > 'Z') {
        cerr << "Invalid name\n";
        exit(1);
    } 
}

Server::Server () : name('?') {}

/* Notice that the keyword static is used in the member function 
declaration but is not used in the member function definition    */

inline int Server::getTurn () { return ++turn; } 

inline bool Server::stillOpen () { return nowOpen; }

void Server::serveOne () { 
    if (nowOpen && lastServed < turn)
        cout << "Server " << name << " now serving " << ++lastServed << endl;

    if (lastServed >= turn) //Everyone served
        nowOpen = false;
}