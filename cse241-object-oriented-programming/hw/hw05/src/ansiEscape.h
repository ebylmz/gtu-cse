#ifndef ANSI_ESCAPE_H
#define ANSI_ESCAPE_H

namespace AnsiEscape {
    enum class Color {normal, red, green, yellow, blue, magenta, cyan, white};

    void clearScreen ();

    void setCursor (int row = 1, int col = 1);
    // top-left corner's coordinate is row = 1, col = 1

    void setFontColor (Color c = Color::normal);

    void setBackgroundColor (Color c = Color::normal);
}

#endif