#include <iostream>
#include <string>
#include "boardGame2D.h"
#include "eightPuzzle.h"
#include "ansiEscape.h"

using std::cout;
using std::string;
using std::endl;
using AnsiEscape::setCursor;
using AnsiEscape::clearScreen;

namespace {
    void swap (int & i, int & j) {
        int tmp = i;
        i = j;
        j = tmp;
    }

    int strToInt (const string & str) {
        int r = 0;
        for (int i = 0; i < str.size() && isdigit(str[i]); ++i)
            r = r * 10 + str[i] - '0';
        return r;
    }

    string intToStr (int n) {
        string str;
        int d = (n < 0) ? -n : n;

        do {
            str.push_back('0' + d % 10);
            d /= 10;
        } while (d > 0);

        for (int i = 0; i < str.size() / 2; ++i) {
            char tmp = str[i];
            str[i] = str[str.size() - i - 1];
            str[str.size() - i - 1] = tmp; 
        }

        return (n < 0) ? "-" + str : str;
    }
}

namespace GameEBY {
    EightPuzzle::EightPuzzle () : BoardGame2D(), _lastMoved(0) {
        initialize();
    }
    //! NOT IMPLEMENTED YET

    void EightPuzzle::initialize () {        
        // int 0 indicates empty cellValue
        _board.resize(SIZE_PUZZLE);
        for (int i = 0; i < SIZE_PUZZLE; ++i) {
            _board[i].resize(SIZE_PUZZLE);
            for (int j = 0; j < SIZE_PUZZLE; ++j)
                _board[i][j] = i * SIZE_PUZZLE + j;
        }
        // shuffle the game board and update the game status
        shuffle();
        _lastMoved = 0;
        setNumOfMov(0);
        setBoardScore(0);
    }

    int EightPuzzle::playUser (const string & mov) {
        int rvalue = SUCCESS;   // init value
        
        int cellValue = strToInt(mov);    // location
        if (cellValue == -1) 
            rvalue = BAD_INPUT;
        else {
            int sRow;
            int sCol;
            // find the location of the cell given by it's value
            if (location(cellValue, sRow, sCol)) {
                int eRow = sRow;
                int eCol = sCol;

                // if possible define a movement
                if (0 <= sRow - 1 && _board[sRow - 1][sCol] == 0)
                    --eRow; // up movement
                else if (sRow + 1 < _board.size() && _board[sRow + 1][sCol] == 0) 
                    ++eRow; // down movement
                else if (0 <= sCol - 1 && _board[sRow][sCol - 1] == 0) 
                    --eCol; // left movement
                else if (sCol + 1 < _board[sRow].size() && _board[sRow][sCol + 1] == 0) 
                    ++eCol; // right movement
                else
                    rvalue = INVALID_MOV;

                // apply the movement
                if (rvalue != INVALID_MOV) {
                    // apply movement    
                    _lastMoved = _board[eRow][eCol] = _board[sRow][sCol];
                    _board[sRow][sCol] = 0;
                    // update the number of movement
                    setNumOfMov(numOfMov() + 1);
                }
            }
            else
                rvalue = INVALID_MOV;
        }
        return rvalue;
    }

    bool EightPuzzle::playAuto () {
        vector<string> v = nextMoves();
        if (v.size() > 0) {
            srand(time(NULL));
            
            // dont make the pre movement o.w. infinite loop can occur 
            int i;
            do i = rand() % v.size();
            while (strToInt(v[i]) == _lastMoved);

            return playUser(v[i]);
        }
        return false;
    }

    vector<string> EightPuzzle::nextMoves () {
        vector<string> possibleMoves;

        // find the location of empty cell
        int row, col;
        location(0, row, col);
        
        // check the 4 direction
        if (0 <= row - 1)                   // up movement
            possibleMoves.push_back(intToStr(_board[row - 1][col])); 
        
        if (row + 1 < _board.size())        // down movement 
            possibleMoves.push_back(intToStr(_board[row + 1][col])); 
        
        if (0 <= col - 1)                   // left movement 
            possibleMoves.push_back(intToStr(_board[row][col - 1])); 
        
        if (col + 1 < _board[row].size())   // right movement
            possibleMoves.push_back(intToStr(_board[row][col + 1])); 

        return possibleMoves;
    }
    
    bool EightPuzzle::endGame () const {
        int n = 1;
        for (int i = 0; i < SIZE_PUZZLE; ++i)
            for (int j = 0; j < SIZE_PUZZLE; ++j)
                if (_board[i][j] != n++ && !(i == SIZE_PUZZLE - 1 && j == SIZE_PUZZLE - 1))
                    return false;
        return true;
    }    
    
    void EightPuzzle::print (ostream & outs) const {
        clearScreen();
        setCursor(1, 1);
        outs << *this << endl;
        outs << "move: " << numOfMov() << endl;
    }

    ostream & operator<< (ostream & outs, const EightPuzzle & game) {      
        string border;  // horizontal border

        for (int i = 0, N = SIZE_PUZZLE * 4 + 1; i < N; ++i)
            border.push_back('-');
        // 0 indicates empty cell
        for (int i = 0; i < game._board.size(); ++i) {
            outs << border << endl;
            for (int j = 0; j < game._board[i].size(); ++j) {
                outs << "| ";  
                if (game._board[i][j] == 0)
                    outs << "  ";
                else
                    outs << game._board[i][j] << " ";
            }
            cout << "|\n";
        }
        outs << border << endl;
        return outs;   
    }

    bool EightPuzzle::location (int cellValue, int & row, int & col) const {
        if (0 <= cellValue && cellValue < SIZE_PUZZLE * SIZE_PUZZLE)
            for (row = 0; row < _board.size(); ++row)
                for (col = 0; col < _board[row].size(); ++col)
                    if (_board[row][col] == cellValue)
                        return true;
        return false;
    }

    int EightPuzzle::calcScore () const {
        int score = 0;
        for (int i = 0, cellValue = 1; i < _board.size(); ++i)
            for (int j = 0; j < _board[i].size(); ++j)
                if (_board[i][j] != cellValue++)
                    ++score;
        return score;
    }

    void EightPuzzle::shuffle () {
        srand(time(NULL));
        int N = SIZE_PUZZLE * SIZE_PUZZLE;
        int r1, r2;
        int i1, i2, j1, j2;

        int n = rand() % 10 + 10;    // make at least 10 random swap
        while (n > 0 || !solvable() || endGame()) {   
            r1 = rand() % N; 
            r2 = rand() % N; 
            i1 = r1 / SIZE_PUZZLE;
            j1 = r1 % SIZE_PUZZLE;
            i2 = r2 / SIZE_PUZZLE;
            j2 = r2 % SIZE_PUZZLE;
            swap(_board[i1][j1], _board[i2][j2]);
            --n;
        }
    }

    int EightPuzzle::inversionOf (int row, int col) const {
        int inversion = 0;
        if (_board[row][col] != 0)
            for (int i = row; i < SIZE_PUZZLE; ++i)
                for (int j = (i == row) ? col + 1 : 0; j < SIZE_PUZZLE; ++j)
                    if (_board[i][j] != 0 && _board[row][col] > _board[i][j])
                        ++inversion;
        return inversion;
    }

    bool EightPuzzle::solvable () const {
        // cellValue 0 indicates an empty cell
        int totalInversion = 0;
        for (int i = 0; i < SIZE_PUZZLE; ++i)
            for (int j = 0; j < SIZE_PUZZLE; ++j)
                totalInversion += inversionOf(i, j);
        
        // cout << "Inversion number: " << totalInversion << endl;
        // std::cin.get();
        
        // if totalInversion is even puzzle is solvable
        return totalInversion % 2 == 0;   
    }
}