#ifndef EIGHT_PUZZLE_H
#define EIGHT_PUZZLE_H

#include <iostream>
#include <vector>
#include "boardGame2D.h"

using std::vector;

namespace GameEBY {
    const int SIZE_PUZZLE = 3; // 3x3 puzzle

    class EightPuzzle : public BoardGame2D {
    public:
        EightPuzzle ();
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
        // checks if given movement is valid for current board
        friend ostream & operator<< (ostream & outs, const EightPuzzle & game);      
        // checks if there is a possible movement at given cell
    private:
        bool location (int cellValue, int & row, int & col) const;
        // returns the location of given cell value as row and col
        // before using row and col check if function returns true or false
        int calcScore () const;
        // calculates the game score
        void shuffle ();
        // shuffles the game board 
        bool solvable () const;
        // checks if created puzzle is solvable
        int inversionOf (int row, int col) const;
        // counts the the number of inversion for given cell
        
        int _lastMoved; 
        // keeps the cellValue of last move to prevent doing pre movement in auto mode
        vector<vector<int>> _board;
        // prefer vector instead of fix sized arr for future improvments
    };
}

#endif