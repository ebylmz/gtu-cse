#ifndef pegSolitaire
#define pegSolitaire

//! Should inline functions in header?
// YESw

#include <iostream>
#include <vector>
#include <string>
#include <fstream>
#include <ctime>
#include <cstdlib>
using namespace std;

const int RETURN_SUCCESS =  0;  // return value for successfull execution
const int RETURN_FAILURE = -1;  // return value for unsuccessfull execution
const int RETURN_EXIT    = -2;  // return value for aborted execution by user

/***********************************************************************************
 *                                  PegSolitaire
 **********************************************************************************/

class PegSolitaire {
public:
    enum class Direction : char {undefined = 'N', up = 'U', down = 'D', right = 'R', left = 'L'};
    enum class CellValue : char {undefined = 'N',  peg = 'P', empty = '.', junction = '#', wall = ' '};
    enum class GameMode {undefined = 'N', computer = 'C', human = 'H'};
    enum class Command {undefined, load, save, exit};
    class Cell;
    class Movement;

    // PegSolitaire class object cannot be created without initializing the game board
    PegSolitaire (const string & filename);
    // Starts with given board
    PegSolitaire (int boardType);
    // Starts with the specific board type (1 to 6)
    PegSolitaire ();
    // Takes the proper information from user

    void startGame ();
    // Starts the pegSolitaire game

    Movement play ();
    // plays the current game for a single step and returns the movement that made.
    
    void play (const Movement & mov);
    // Takes a cell position and it plays the user

    void playGame ();  
    // plays the game by asking the user the board type first then
    // automatically plays the game until it ends or player wants to exit. 
    
    int applyMovement (const Movement & mov);

    int applyCommand (const string & command);
    // Applies the commands: load, save, exit

    int load (const char * fileName);

    int save (const char * fileName) const;

    bool compare (const PegSolitaire & other);
    // Returns true if the first game has more pegs, otherwise it returns false

    int getNumberOfMov () const;

    int getScore () const;
    
    char getGameMode () const;

    int getBoardRow () const;
    
    int getBoardCol () const;
    
    static int getPegNumber (); 
    // Returns total peg number in all the active games

    static int getActiveGameNumber ();
    // Returns the number of opened game by player

    bool isGameOver ();

    bool getGameActivity () const;
    void setGameActivity (bool activity);

    void displayGame () const;
    // Display related game information and the current board

    void displayGameStatus () const;

    void displayBoard () const;

private:
    CellValue getCellValue (int row, int col) const;
    
    const Cell & getCell (int row, int col) const;
    
    const vector<vector<Cell>> & getBoard () const;

    Cell & jumpCell (const Movement & mov);
    Cell & targetCell (const Movement & mov);

    bool isMovable (const Cell & startCell);
    // Cheks if given cell can make a valid movement
    
    bool isValidMovement (const Movement & mov);
    bool isValidMovement (int startRow, int startCol, Direction dir);
    bool isValidMovement (const Cell & startCell, Direction dir);
    bool isValidMovement (const Cell & start, const Cell & jump, const Cell & target);

    void setGameConfig ();
    // Sets the game configuration for known board types 
    void setGameConfig (int setBoardRow, int setBoardCol);
    // Sets the game configuration for unknown board types which are loaded with given filename

    void setScore ();
    // Sets/Updates the score of current game

    int setBoard ();
    // Takes and set the board type from the user in case of exit returns 0

    void setBoard (int boardType);
    // Sets the game board from existing 6 different board

    void setBoardFrench ();        
    void setBoardGerman ();        
    void setBoardAsymmetrical ();  
    void setBoardEnglish ();       
    void setBoardDiamond ();       
    void setBoardTriangular ();    

    void autoResize (); 
    // Resize the board as NxN square

    void initBoard (int row, int col, CellValue initValue);
    // Allocates enough memory and initialize the board as initValue

    void fillBoard (int outStart, int outEnd, int inStart, int inEnd, CellValue v);
    // fills the given rectangular area with given value v

    static int pegNumber;   // Total peg number in all the active games
    static int gameNumber;  // The number of opened game by player
    int numberOfMovement;
    int score;
    bool activeGame;        // Becomes true when all the game configuration done
    GameMode gameMode;
    string gameName;    //! NOT IMPLEMENTED YET
    vector<vector<Cell>> board;
    int boardRow;      // Since resize function changes the row and col number
    int boardCol;      // to don't lose the original values keep them seperatly
};

/***********************************************************************************
 *                                     Cell
 **********************************************************************************/

class PegSolitaire::Cell {
public:
    // Constructors
    Cell (int cellRow, int cellCol, CellValue CellValue);
    Cell ();

    // Getters
    void getCell (int & cellRow, int & cellCol, CellValue & cellValue) const;
    CellValue getValue () const;
    int getCol () const;
    int getRow () const;

    // Setters returns RETURN_FAIL in case of invalid 
    int setCell (int cellRow, int cellCol, CellValue cellValue);
    int setRow (int cellRow);
    int setCol (int cellCol);
    int setValue (CellValue cellValue);

private:
    // Cell position (row and coloumn) and value
    int col;        
    int row;        
    CellValue value;  // peg, empty, wall
};

/***********************************************************************************
 *                                     Movement
 **********************************************************************************/

class PegSolitaire::Movement {
public:
    Movement (int startRow, int startCol, Direction dir);
    Movement ();

    int setMovement (const string & mov);
    // Sets the movement by asking question to user
    int setStartRow(int startRow);
    int setStartCol(int startCol);
    int setDirection (Direction dir);

    int getStartRow () const;
    int getStartCol () const;
    Direction getDirection () const;

    friend ostream & operator<< (ostream & outStream, const Movement & mov);
    
    string colToStr () const;
    // String to int value
    int colToInt (const string & str_col) const;
    // Int to string value
private:
    int row;    // Start row of movement
    int col;    // Start coloumn of movement
    Direction direction;
};

/***********************************************************************************
 *                                    Helper
 **********************************************************************************/
string setw (int n);
// prints n space 

bool getChoice (string prompt);

int getChoice (string prompt, int lb, int ub);

int getChoice (string inPrompt, string errPrompt, int lb, int ub);

void enterTo (const string & prompt);

int bigger (int n1, int n2);

bool isInRange (int n, int lb, int ub);
// Cheks if given integer is in between lower and upper bounds

bool isDigit (char c);

bool isLetter (char c);

int strToInt (const string & s);

char charUp (char c);

string strUp (const string & s);

PegSolitaire::Command whichCommand(const string & str);

void listGames (const vector<PegSolitaire> & game);

void displayGameRules ();

void displayAllBoardTypes ();
#endif