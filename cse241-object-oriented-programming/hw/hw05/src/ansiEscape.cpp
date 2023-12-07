#include <iostream>
#include "ansiEscape.h"

using std::cout;
using std::endl;

namespace AnsiEscape {
    void clearScreen () {
        cout << "\x1B[2J";
    }

    void setCursor (int row, int col) {
        // printf("\033[%d;%dH", Y, X)
        cout << "\033[" << row << ";" << col << "H";
    }

    void setFontColor (Color c) {
        switch (c) {
            case Color::red:
                cout << "\x1B[1;31m"; break;
            case Color::green:
                cout << "\x1B[1;32m"; break;
            case Color::yellow:
                cout << "\x1B[1;33m"; break;
            case Color::blue:
                cout << "\x1B[1;34m"; break;
            case Color::magenta:
                cout << "\x1B[1;35m"; break;
            case Color::cyan:
                cout << "\x1B[1;36m"; break;
            case Color::white:
                cout << "\x1B[1;37m"; break;
            case Color::normal:
            default:
                cout << "\x1B[0m";
        }
    }

    void setBackgroundColor (Color c) {
        switch (c) {
            case Color::red:
                cout << "\x1B[41m"; break;
            case Color::green:
                cout << "\x1B[42m"; break;
            case Color::yellow:
                cout << "\x1B[43m"; break;
            case Color::blue:
                cout << "\x1B[44m"; break;
            case Color::magenta:
                cout << "\x1B[45m"; break;
            case Color::cyan:
                cout << "\x1B[46m"; break;
            case Color::white:
                cout << "\x1B[47m"; break;
            case Color::normal:
            default:
                cout << "\x1B[0m";
        }
    }
}