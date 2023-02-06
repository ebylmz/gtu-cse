#ifndef PEG_SOLITAIRE_H
#define PEG_SOLITAIRE_H

#include <iostream>
#include <string>
#include <vector>
#include "boardGame2D.h"

using std::vector;
using std::string;

namespace GameEBY {
    enum class PegCell : char {undef = 'N',  peg = 'P', empty = '.', wall = ' '}; 
    
    class PegSolitaire : public BoardGame2D {
    public:
        PegSolitaire ();

        void initialize () override;
        // initialize the game board 
        int playUser (const string & mov) override;
        // plays the game with the given str movement 
        bool playAuto () override;
        // plays the game by the computer for one move
        vector<string> nextMoves () override;
        // returns all possible legal moves for the current board 
        bool endGame () const override;
        // returns true if game is over
        void print (ostream & outs) const override;
        // prints the game informain such as board, number of movements ...
        friend ostream & operator<< (ostream & outs, const PegSolitaire & game);      
        // prints the game board to the given stream
    private:
        bool move (int sRow, int sCol, Direction dir);
        // applies given movement if it's possible
        bool validMovement (int sRow, int sCol, Direction dir) const;
        // checks if given movement is valid for current board
        bool movableCell (int row, int col) const;
        // checks if there is a possible movement at given cell
        bool moveIndex (int & row, int & col, int n, Direction dir) const;
        // gets the end position of movement
        // n is positive int and indicates amount of movement 
        // pre: row and col must be equal to starting position 
        int calcScore () const;
        // calculates the game score 
        vector<vector<PegCell>> _board;
    };
}

#endif