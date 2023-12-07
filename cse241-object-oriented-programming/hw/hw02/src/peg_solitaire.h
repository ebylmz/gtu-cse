/**
 * @file    peg_solitaire.h
 * @author  Emirkan Burak Yilmaz (emirkanyilmaz2019@gtu.edu.tr)
 * @brief   Header File of Peg Solitaire library
 * @version 0.2
 * @date    2021-10-27
 * 
 * @copyright Copyright (c) 2021
 * 
 */

#ifndef peg_solitaire
#define peg_solitaire

using namespace std;

const int RETURN_SUCCESS =  0;  // return value for successfull execution
const int RETURN_FAILURE = -1;  // return value for unsuccessfull execution
const int RETURN_SUDO    = -2;  // return value for stopped execution by user

enum class GameMode {human, computer, undefined, none};

enum class CellState {empty, peg, out};

enum class BoardType {french = 1, german = 2, asymmetrical = 3, english = 4, diamond = 5, triangular = 6};

enum class Direction {up, down, right, left, upRight, upLeft, downRight, downLeft, none};

enum class Command {load, save, exit, none};

/***********************************************************************************
 * Game Management
 **********************************************************************************/

void pegStart ();
// Start the Peg Solitaire game

void startNewGame ();

void continueGame ();

int playComputerMode (vector<vector<CellState>> & board, int numberOfMovement = 0);
int playHumanMode (vector<vector<CellState>> & board, int numberOfMovement = 0);
// Returns the number of moves for successful run and RETURN_FAILURE for unsuccessful execution

int createRandomMovement (const vector<vector<CellState>> & board, int & startRow, int & startCol, Direction & dir);
// In case of succesfull execution returns RETURN_SUCCESS, on the other hand
// returns RETURN_FAILURE when the game is over and there is no valid movement remains

int getMovement (const string & mov, int & startRow, int & startCol, Direction & dir);
// Reads the movement from console and returns the movement as 
// indexes of the start point and the direction of movement

Direction getDirection (const string & movement);
// Returns the Obtained direction information inside of the movement 

string dirToStr (Direction dir);
// Generate a string indicate an direction. Exp: U, D, L, R, UR ...

int applyMovement (vector<vector<CellState>> & b, int startRow, int startCol, Direction dir);
// Applies the movement, for invalid operation returns RETURN_FAILURE

void moveUp (int & jumpRow, int & targetRow);
void moveDown (int & jumpRow, int & targetRow);
void moveRight (int & jumpCol, int & targetCol);
void moveLeft (int & jumpCol, int & targetCol);
// Pre: jump and target Cells are should be initialized from start Cell

int getMoveCell (const vector<vector<CellState>> & board, int startRow, int startCol, Direction dir, int & jumpRow, int & jumpCol, int & targetRow, int & targetCol);
// Produce the movement cells (start, jump and target) from given start cell 

bool isMovable (const vector<vector<CellState>> & board, int startRow, int startCol);
// Checks if given board cell is movable or not

bool isMovable (const vector<vector<CellState>> & board, int startRow, int startCol, Direction dir);
// Checks if given board cell is movable or not for specific direction

bool isProperDirection (const vector<vector<CellState>> & board, Direction dir);

bool isTriangularMovement (Direction dir);

bool isDiagonalMovement (Direction dir);

bool isGameOver (const vector<vector<CellState>> & board);

int calculateScore (const vector<vector<CellState>> & board);

void showGameRules ();
// Prints the rule of the games

void showGameResult (int score, int numberOfMovement);

void welcomeGreet();
// Prints welcome message for the gamer

/***********************************************************************************
 * Board Start
 **********************************************************************************/

void initBoard (vector<vector<CellState>>& b, BoardType btype);

void initBoardFrench(vector<vector<CellState>> & board);        

void initBoardGerman(vector<vector<CellState>> & board);        

void initBoardAsymmetrical(vector<vector<CellState>> & board);  

void initBoardEnglish(vector<vector<CellState>> & board);       

void initBoardDiamond(vector<vector<CellState>> & board);       

void initBoardTriangular(vector<vector<CellState>> & board);    

void createBoard (vector<vector<CellState>> & b, int row, int col, CellState c);
// Creates a board as given dimension and initiliaze all the board with c

void showGameStatus (const vector<vector<CellState>> & board, int numberOfMoves, GameMode gm);
// Print to the screen current game status(number of moves etc.) and the current board

void showBoard (const vector<vector<CellState>> & b);
// Prints the curent status of board

void showTriangularBoard (const vector<vector<CellState>> & b);

void showNonTriangularBoard (const vector<vector<CellState>> & b);

void printn (char c, int n);
// Prints the given char for n times to the current line  

char cellStateToChar (CellState cs);
// Converts CellState type to it's char equivalent

bool isInBoard (const vector<vector<CellState>> & b, int row, int col);
// Checks if referred cell with it's row and col is inside of the board

bool isTriangularBoard (const vector<vector<CellState>> & b);

void printAllBoardTypes ();
// Prints 6 different type of boards

/***********************************************************************************
 * Load(Import) & Save(Export)  
 **********************************************************************************/
int saveGame (const vector<vector<CellState>> & board, const char * fileName, GameMode gm, int numberofMoves);

int loadGame (vector<vector<CellState>> & board, const char * fileName, GameMode & gm, int & numberOfMoves);

int importBoard (vector<vector<CellState>> & b, ifstream & inStream);

void exportBoard(const vector<vector<CellState>> & b, ofstream & outStream);

Command whichCommand (const string & s);
Command whichCommand (const char * s);
// Returns the type pf the command (exit, load etc.)

/***********************************************************************************
 * Utility
 **********************************************************************************/
bool getChoice (string prompt);
// Prompt the user a Y/N question and returns it's value Y for true, N for false  

int getChoice (string prompt, int lb, int ub);
// Prompt the user and returns its value specified with range [lb, ub] 

int getChoice (string inPrompt, string errPrompt, int lb, int ub);
// Prompt the user and returns its value specified with range [lb, ub]
// In case of invalid input alerts given errPrompt 

string BoardTypeToStr (BoardType btype);
// Returns the name of the given board 

int strToInt (string & s);
// Converts string to integer value 
// Returns RETURN_FAILURE existance of nondigit charachter  

char gameModeToChar (GameMode gm);

GameMode charToGameMode (char gm);

void strToCstr (const string & str, char * cstr);
// Precondition: cstr array has at least n space (n = str.lenght() + 1)

int cstrlen (const char * s);

void showNextPageEffect ();

bool isInRange (int n, int lb, int ub);
// Checks if given integer n in inside of the boundry [lb, ub]

bool isDigit (char c);

bool isLetter (char c);

char upperCase (char c);

void convertUpperCase (string & s);

#endif 