#ifndef KLOTSKI_H
#define KLOTSKI_H

#include <iostream>
#include <string>
#include <vector>
#include "boardGame2D.h"
#include "ansiEscape.h"

using AnsiEscape::Color;
using std::string;

namespace GameEBY {
    class Klotski : public BoardGame2D {
    public:
        class Block {
        public:
            Block (int startRow = 0, int startCol = 0, int endRow = 0, int endCol = 0, Color color = Color::normal, string name = "  ");

            int startRow () const;
            int startCol () const;
            int endRow () const;
            int endCol () const;
            Color color () const;
            string name () const;

            void setStartPosition (int row, int col);
            void setEndPosition (int row, int col);
            void setColor (Color color);
            void setName (string name);

            void print () const;
            // print the block to the screen
            bool overlap (const Block & other) const;
            // original block and the moved are of course overlap if it's the case 
            bool inside (const Block & outer) const;
            // checks if this block is inside of the outer block
            bool operator== (const Block & other) const;
        private:
            // top left of block in board is position 
            int _sRow;      // start row
            int _sCol;      // start col
            int _eRow;      // end row
            int _eCol;      // end col
            Color _color;   
            string _name;  
        }; // end of class Block
            
        Klotski ();

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
        friend ostream & operator<< (ostream & outs, const Klotski & game);      
        // checks if there is a possible movement at given cell
    private:
        void printBoardBorder() const; 
        // prints the border of board
        bool strToMov (const string & str, int & index, Direction & dir) const;
        // converts given string to movement
        int block (const string & name) const;
        // searchs and returns the index of target block by given it's name
        // in case of nothing find returns -1 
        bool validMovement (const Block & target, Direction dir);
        // checks if given block can make a valid movement 
        bool move (int index, Direction dir);
        // takes the index of target block in _blocks vector
        // applies given movement if it's possible

        Block _border;              // border of current board 
        Block * _lastMoved;         // keeps the address of last moved block 
        vector<Block> _blocks;      // keeps the blocks
    }; 
}

#endif