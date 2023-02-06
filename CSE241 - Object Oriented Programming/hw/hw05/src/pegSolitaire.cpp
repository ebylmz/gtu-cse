#include <iostream>
#include "ansiEscape.h"
#include "pegSolitaire.h"

using std::cout;
using std::cin;
using std::cerr;
using std::endl;
using AnsiEscape::setCursor;
using AnsiEscape::clearScreen;

namespace {
    char charUp (char c) {
        return ('a' <= c && c <= 'z') ? 'A' + c - 'a' : c;
    }
    
    string strUp (const string & s) {
        string r;
        for (int i = 0; i < s.size(); ++i)
            r.push_back(charUp(s[i]));
        return r;
    }
    
    bool isDigit (char c) {return '0' <= c && c <= '9';}
   
    bool isLetter (char c) {
        c = charUp(c);
        return 'A' <= c && c <= 'Z';
    }
    
    bool strToMov (const string & str, int & row, int & col, GameEBY::Direction & dir) {
        string mov(strUp(str));

        // example movement: B2-U
        if (mov.size() == 4 && isLetter(mov[0]) && isDigit(mov[1]) && mov[2] == '-' &&
        (mov[3] == 'U' || mov[3] == 'D' || mov[3] == 'R' || mov[3] == 'L')) {
            col = mov[0] - 'A';
            row = mov[1] - '1';
            dir = static_cast<GameEBY::Direction>(mov[3]);
            return true;
        }
        return false;
    }

    string movToStr (int row, int col, GameEBY::Direction dir) {
        string r;
        r.push_back('A' + col);
        r.push_back('1' + row);
        r.push_back('-');
        r.push_back(static_cast<char>(dir));
        return r;
    }
}

namespace GameEBY {
    PegSolitaire::PegSolitaire () : BoardGame2D() {
        initialize();
    }

    void PegSolitaire::initialize () {
        // construct the 9x9 German Board 
        _board.resize(9);
        for (int i = 0; i < 9; ++i)
            _board[i].resize(9);

        for (int i = 0; i < 3; ++i) {
            for (int j = 0; j < 3; ++j)
                _board[i][j] = PegCell::wall;
            for (int j = 3; j < 6; ++j)
                _board[i][j] = PegCell::peg;
            for (int j = 6; j < 9; ++j)
                _board[i][j] = PegCell::wall;
        }

        for (int i = 3; i < 6; ++i)
            for (int j = 0; j < 9; ++j)
                _board[i][j] = PegCell::peg;
        _board[4][4] = PegCell::empty;

        for (int i = 6; i < 9; ++i) {
            for (int j = 0; j < 3; ++j)
                _board[i][j] = PegCell::wall;
            for (int j = 3; j < 6; ++j)
                _board[i][j] = PegCell::peg;
            for (int j = 6; j < 9; ++j)
                _board[i][j] = PegCell::wall;
        }
        setBoardScore(calcScore());
    }
    
    int PegSolitaire::playUser (const string & mov) {
        int rvalue;
        if (endGame())
            rvalue = GAME_IS_OVER;
        else {
            int row, col;
            Direction dir;
            // if given string is suitable movement format
            // move if it's valid movement and update the game 
            if (!strToMov(mov, row, col, dir)) 
                rvalue = BAD_INPUT;
            else {
                if (!move(row, col, dir))
                    rvalue = INVALID_MOV;
                else 
                    rvalue = SUCCESS;
            }
        }
            
        return rvalue;
    }

    bool PegSolitaire::playAuto () {
        if (!endGame()) {
            vector<Direction> v;
            // selects a random starting location and try each 
            // location respectivly to find a valid random movement 
            srand(time(NULL));
            int row = rand() % _board.size();
            int col = rand() % _board[row].size();

            for (int i = 0; i < _board.size(); ++i) {
                for (int j = 0; j < _board[i].size(); ++j) {
                    // try randomly each direction at selected cell
                    randDirection(v);  

                    for (Direction & d : v)
                        if (move (row, col, d))
                            return true;
                    // select next col
                    col = (col + 1 == _board[row].size()) ? 0 : col + 1;
                }
                // select next row
                row = (row + 1 == _board.size()) ? 0 : row + 1;
            }
        }
        return false;
    }
    
    vector<string> PegSolitaire::nextMoves () {
        vector<string> possibleMoves;
        vector<Direction> v;

        for (int i = 0; i < _board.size(); ++i)
            for (int j = 0; j < _board[i].size(); ++j) {
                if (_board[i][j] == PegCell::peg) {
                    // try each randomly selected direction
                    randDirection(v);
                    for (Direction & d : v)
                        if (validMovement(i, j, d))
                            possibleMoves.push_back(movToStr(i, j, d));
                }
            }
        return possibleMoves;
    }

    bool PegSolitaire::endGame () const {
        for (int i = 0; i < _board.size(); ++i)
            for (int j = 0; j < _board[i].size(); ++j)
                if (movableCell(i, j)) 
                    return false;
        return true;
    }

    void PegSolitaire::print (ostream & outs) const {
        setCursor(1, 1);
        clearScreen();
        outs << *this << endl;
        outs << "move: " << numOfMov() << " " << "score: " << boardScore() << endl;
    }

    ostream & operator<< (ostream & outs, const PegSolitaire & game) {
        outs << " " << " " << " " << " ";
        for (int i = 0; i < game._board.size(); ++i)
            outs << static_cast<char>('A' + i) << " ";
        outs << "\n\n";
        for (int i = 0; i < game._board.size(); ++i) {
            cout << i + 1 << " " << " " << " ";
            for (int j = 0; j < game._board[i].size(); ++j)
                outs << static_cast<char>(game._board[i][j]) << " ";
            outs << endl;
        }
        return outs; 
    }

    bool PegSolitaire::move (int sRow, int sCol, Direction dir) {
        int r = false;
        // Peg board is square, so both row and column boundries are same
        if (0 <= sRow && sRow < _board.size() && 0 <= sCol && sCol < _board.size()) {
            // check the data of start cell 
            if (_board[sRow][sCol] == PegCell::peg) {
                // precondition of moveIndex function
                int eRow = sRow;
                int eCol = sCol;
                
                // get the index of end position of movement 
                if (moveIndex(eRow, eCol, 2, dir)) {
                    // precondition of moveIndex function
                    int jRow = sRow;
                    int jCol = sCol;
                    moveIndex(jRow, jCol, 1, dir);

                    // check datas of end and jump cells
                    if (_board[jRow][jCol] == PegCell::peg && _board[eRow][eCol] == PegCell::empty) {
                        // apply movement
                        _board[sRow][sCol] = PegCell::empty;
                        _board[jRow][jCol] = PegCell::empty;
                        _board[eRow][eCol] = PegCell::peg;
                        // update the number of movement and score
                        setNumOfMov(numOfMov() + 1);
                        setBoardScore(boardScore() - 1);
                        r = true;
                    }
                }                
            }
        }
        return r;
    }

    bool PegSolitaire::validMovement (int sRow, int sCol, Direction dir) const {
        int r = false;
        // Peg board is square, so both row and column boundries are same
        if (0 <= sRow && sRow < _board.size() && 0 <= sCol && sCol < _board.size()) {
            // check the data of start cell 
            if (_board[sRow][sCol] == PegCell::peg) {
                // precondition of moveIndex function
                int eRow = sRow;
                int eCol = sCol;
                
                // get the index of end position of movement 
                if (moveIndex(eRow, eCol, 2, dir)) {
                    // precondition of moveIndex function
                    int jRow = sRow;
                    int jCol = sCol;
                    moveIndex(jRow, jCol, 1, dir);

                    // check datas of end and jump cells
                    if (_board[jRow][jCol] == PegCell::peg && _board[eRow][eCol] == PegCell::empty) 
                        r = true;
                }                
            }
        }
        return r;
    }
    
    bool PegSolitaire::movableCell (int row, int col) const {
        return  validMovement(row, col, Direction::up)   ||
                validMovement(row, col, Direction::down) ||
                validMovement(row, col, Direction::left) ||
                validMovement(row, col, Direction::right);
    }

    bool PegSolitaire::moveIndex (int & row, int & col, int n, Direction dir) const {
        int r = false;

        if (0 <= row && row < _board.size() && 0 <= col && col < _board[row].size()) {
            if (dir == Direction::up) {
                if (0 <= row - n) {
                    row -= n;
                    r = true;
                }
            }
            else if (dir == Direction::down) {
                if (row + n < _board.size()) {
                    row += n;
                    r = true;
                }
            } 
            else if (dir == Direction::left) {
                if (0 <= col - n) {
                    col -= n;
                    r = true;
                }
            }
            else if (dir == Direction::right) {
                if (col + n < _board[row].size()) {
                    col += n;
                    r = true;
                }
            }
        }
        return r; 
    }

    int PegSolitaire::calcScore () const {
        // count the number of peg in game board
        int score = 0;
        for (int i = 0; i < _board.size(); ++i)
            for (int j = 0; j < _board[i].size(); ++j)
                if (_board[i][j] == PegCell::peg)
                    ++score;
        return score;
    }
}