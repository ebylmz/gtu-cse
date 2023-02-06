#include <iostream>

// signature of both message() functions are same
// but this does not cause an error.

namespace Hello {
    void message ();
}

namespace GoodBye {
    void message ();
}

void message ();


int main (void) {
    using GoodBye::message;
    message();

    {
        using Hello::message;
        message();

        GoodBye::message();
    }
    Hello::message();
}

namespace Hello {
    void message () {
        std::cout << "Hello\n";
    }
}

namespace GoodBye {
    void message () {
        std::cout << "Goodbye\n";
    }
}

void message () {
    std::cout << "Global message\n";
}