#ifndef BOARD_GAME_2D_H
#define BOARD_GAME_2D_H

#include <iostream>
#include <string>
#include <vector>

using std::ostream; 
using std::vector; 
using std::string; 

namespace GameEBY {
    enum class Direction : char {undef = 'N', up = 'U', down = 'D', right = 'R', left = 'L'};

    const int SUCCESS = 0;          // successful execution
    const int BAD_INPUT = 1;        // invalid format input
    const int INVALID_MOV = 2;      // invalid movement
    const int GAME_IS_OVER = 3;     
    // function return values

    void randDirection (vector<Direction> & v);
    // modifies given Direction vector as eachy direction as random index

    class BoardGame2D {
    public:
        BoardGame2D ();
        // constructor
        virtual void initialize () = 0;
        // initialize the game board
        virtual int playUser (const string & mov) = 0;
        // applies the given movement 
        virtual void playUser () final;
        // takes and applies the movement in a loop till game is over or user exit 
        virtual bool playAuto () = 0;
        // plays the game by the computer for one move.
        virtual void playAutoAll () final;
        // plays the game until it is over
        static void playVector (vector<BoardGame2D *> & gameV);
        // plays all the games in the vector until they end
        virtual vector<string> nextMoves () = 0;
        // returns all possible legal moves for the current board 
        int numOfMov () const;
        // returns the number of movement made
        int boardScore () const;
        // returns the score of current game
        virtual bool endGame () const = 0;
        // returns true if game is over
        virtual void print (ostream & outs) const = 0;
        // prints the game information such as board, number of movements ...
        friend ostream & operator<< (ostream & outs, const BoardGame2D & game);        
        // prints the game board to the given stream
    protected:
        virtual void printRvalue (int errCode);
        // prints the function return meaning 
        // this function implemented as virtual 
        // to provide override property for derived classes 

        // setter functions for derived classes
        void setBoardScore (int score);
        void setNumOfMov (int numOfMov);
    private:    
        int _numOfMov;      // number of movement made
        int _boardScore;  
    };
}

#endif