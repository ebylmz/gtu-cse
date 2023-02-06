/**
 * @file    peg_solitaire.cpp
 * @author  Emirkan Burak Yilmaz (emirkanyilmaz2019@gtu.edu.tr)
 * @brief   Implementation of Peg Solitaire library
 * @version 0.2
 * @date    2021-10-27
 * 
 * @copyright Copyright (c) 2021
 * 
 */

#include <iostream>
#include <vector>
#include <string>
#include <fstream>
#include <cstdlib>
#include <ctime>
#include <cmath>
#include "peg_solitaire.h"

using namespace std;  

/***********************************************************************************
 * Game Management
 **********************************************************************************/

void pegStart () {
    bool exit = false;
    int choice;

    welcomeGreet();
    cout << "\nEnter to start ";
    cin.get();
    showGameRules();

    do {
        cout << "\n============ Main Menu ============\n"
             << "0. Exit\n"
             << "1. Start New Game\n"
             << "2. Continue Game\n";
        choice = getChoice("Choose: ", 0, 2);
        switch (choice) {
            case 0:
                exit = getChoice("\nAre you sure you want to exit the game? (y or n) "); break;
            case 1:
                startNewGame(); break;
            case 2:
                continueGame(); break;
        }
    } while (! exit);
    cout << "======================== EXIT ========================\n";
}

void startNewGame () {
    vector<vector<CellState>> board;
    BoardType btype;
    int choice;

    // Ask the board type and initialize the board
    printAllBoardTypes();
    cout << "0. Exit\n"
         << "1. French\n"
         << "2. German\n"
         << "3. Asymmetrical\n"
         << "4. English\n"
         << "5. Diamond\n"
         << "6. Triangle\n"
         << "7. Random Selection\n";
    choice = getChoice("Select your board: ", 0, 7);
    
    // Select a random board
    if (choice == 7) {
        srand(time(NULL));
        choice = rand() % 6 + 1;
        string randBoard = BoardTypeToStr(static_cast<BoardType>(choice));
        cout << endl << randBoard << " board selected randomly\n";
    }

    btype = static_cast<BoardType>(choice);
    initBoard(board, btype);

    // There are two types of game: Human & Computer
    showNextPageEffect();
    cout << "0. Return Main Menu\n"
         << "1. Human Game\n"
         << "2. Computer Game\n";
    choice = getChoice("Select the game type: ", 0, 2);

    if (choice != 0) {
        showNextPageEffect();
        decltype(choice) numberOfMovement = 0; // int numberOfMovement = 0;
        switch (choice) {
            case 1:
                numberOfMovement = playHumanMode(board);    break;
            case 2:
                numberOfMovement = playComputerMode(board); break;
        }
        showGameResult(calculateScore(board), numberOfMovement);
        cout << "Enter to continue ";
        cin.get();
    }
}

void continueGame () {
    vector<vector<CellState>> board;
    ifstream inStream;
    int numberOfMovement;
    int r;
    GameMode gm(GameMode::undefined);   // Initialy gamemode undefined

    // Load the game configuration 
    do {
        char command[100];
        cout << "\n\nEnter your file name: ";
        cin >> command;
        // Check if user enter command exit
        if (whichCommand(command) == Command::exit)
            r = RETURN_SUDO;
        else
            r = loadGame(board, command, gm, numberOfMovement);
    } while (r == RETURN_FAILURE);

    if (r != RETURN_SUDO) {
        cout << "Your game is loaded\n";
        showNextPageEffect();
        switch (gm) {
            case GameMode::computer: 
                numberOfMovement = playComputerMode(board, numberOfMovement);  break;
            case GameMode::human: 
                numberOfMovement = playHumanMode(board, numberOfMovement);     break;
        }
        showGameResult(calculateScore(board), numberOfMovement);
        cout << "Enter to continue ";
        cin.get();
    }
}

int playHumanMode (vector<vector<CellState>> & board, int numberOfMovement) {
    int r;
    GameMode gm(GameMode::human);

    showGameStatus(board, 0, gm);

    do {
        int startRow, startCol;
        char fileName[100];
        bool exit = false;
        string strMov, command;
        Direction dir;

        cout << "\nMovement or Command\n"
             << ">> ";
        cin >> command;
        strMov = command;
        convertUpperCase(strMov);

        if (getMovement(strMov, startRow, startCol, dir) == RETURN_SUCCESS) {
            r = applyMovement(board, startRow, startCol, dir);
            if (r == RETURN_SUCCESS)
                showGameStatus(board, ++numberOfMovement, gm);
            else {
                cerr << "(!) Invalid movement\n";
                r = RETURN_FAILURE;
            }
        }
        else if (whichCommand(command) == Command::save) {
            cin >> fileName;
            r = saveGame(board, fileName, gm, numberOfMovement);
            if (r == RETURN_SUCCESS) {
                cout << "Game is saved successfuly\n";
                showNextPageEffect();
            }
        }
        else if (whichCommand(command) == Command::load) {
            cin >> fileName;
            r = loadGame(board, fileName, gm, numberOfMovement);
            if (r == RETURN_SUCCESS) {
                cout << "Game loaded successfuly\n";
                showNextPageEffect();
                showGameStatus(board, numberOfMovement, gm);
            }
            else
                r = RETURN_FAILURE;
        }
        else if (whichCommand(command) == Command::exit) {
            if (getChoice("Do you want to save your progress? (y or n) ")) {
                cout << "Enter the file name: ";
                cin >> fileName;
                if (whichCommand(fileName) != Command::exit)
                    saveGame(board, fileName, gm, numberOfMovement);
            }
            r = RETURN_SUDO;
        }
        else {
            cerr << "(!) " << command << " command not found\n";
            r = RETURN_FAILURE;
        }
    } while (r != RETURN_SUDO && (r == RETURN_FAILURE || isGameOver(board) == false));
    return numberOfMovement;
}

int playComputerMode (vector<vector<CellState>> & board, int numberOfMovement) {
    int startRow, startCol; 
    GameMode gm(GameMode::computer);
    Direction dir;
    bool exit = false;

    showGameStatus(board, 0, gm);
    while (createRandomMovement(board, startRow, startCol, dir) == RETURN_SUCCESS && !exit) {
        // Print the movement will apply by computer
        cout << "\nComputer Movement: " << static_cast<char>('A' + startCol) << static_cast<char>('1' + startRow) << '-' << dirToStr(dir)
             << " (enter to continue)\n"
             << ">> ";
        
        char c;
        string command; 

        // if user just enter this for loop does not execute
        for (cin.get(c); c != '\n'; cin.get(c))
            command.push_back(c);

        if (command.length() == 0) {    // User just enter (c = '\n')
            applyMovement(board, startRow, startCol, dir);
            showGameStatus(board, ++numberOfMovement, gm);
        }
        else {
            string s;
            char fname[100];
            Command commandType = whichCommand(command);

            switch (commandType) {
                case Command::exit:
                    if (getChoice("Do you want to save your progress? (y or n) ")) {
                        cout << "Enter the file name: ";
                        cin >> fname;
                        cin.get();  // Take the new line chararchter
                        saveGame(board, fname, gm, numberOfMovement);
                    }
                    exit = true;
                    break;
                case Command::load:
                case Command::save:
                    //! Optimization needed for commands contain multiple spaces
                    if (command.length() >= 5) {
                        strToCstr(command.substr(5), fname);
                        if (commandType == Command::load) {
                            loadGame(board, fname, gm, numberOfMovement);
                            showNextPageEffect();
                        }
                        else {
                            showNextPageEffect();
                            saveGame(board, fname, gm, numberOfMovement);
                        }
                        showGameStatus(board, numberOfMovement, gm);
                    }
                    break;
                case Command::none:
                    cerr << "(!) " << command << " command not found\n";
            }
        }
    }
    return numberOfMovement;
}

int createRandomMovement (const vector<vector<CellState>> & board, int & startRow, int & startCol, Direction & dir) {
    int r;
    if (isGameOver(board) == false) {
        srand(time(NULL));
        do {
            // Select random position at the board
            startRow = rand() % board.size();
            startCol = rand() % board[startRow].size(); 

            // Select a proper random direction
            if (isTriangularBoard(board))
                //  Up & Down movements are invalid, so range would be (right = 2 to downleft=7)
                dir = static_cast<Direction>((rand() % 6) + 2);
            else
                dir = static_cast<Direction>(rand() % 4);
        } while (isMovable(board, startRow, startCol, dir) == false);
        r = RETURN_SUCCESS; 
    }
    else
        r = RETURN_FAILURE;
    return r;
}

int getMovement (const string & mov, int & startRow, int & startCol, Direction & dir) {
    int r = RETURN_FAILURE;
    // Check if given movement is in a proper format
    if ((mov.length() == 4 || mov.length() == 5) && isLetter(mov[0]) && isDigit(mov[1]) && mov[2] == '-') {
        startCol = mov[0] - 'A';
        startRow = mov[1] - '1';

        dir = getDirection(mov);
        if (dir != Direction::none) 
            r = RETURN_SUCCESS; 
    }
    return r;
}

Direction getDirection (const string & movement) {
    Direction dir = Direction::none;

    if (movement.length() == 4 || movement.length() == 5) {
        int x = 0, y = 0;
        bool err = false;
        
        /*  To find the direction, use 1x1 cordinate system. If given
            directions resulted in at orijin or out of cordinate
            system, then given directions are invalid.                */ 
        for (int i = 3; i < movement.length() && err == false; ++i) {
            switch (movement[i]) {
                case 'U':
                    ++y; break;
                case 'D':
                    --y;
                    break;
                case 'R':
                    ++x; break;
                case 'L':
                    --x; break;
                default:
                    err = true;
            }
        } 

        if (err == false && (x != 0 || y != 0) && isInRange(abs(x), 0, 1) && isInRange(abs(y), 0, 1)) {
            if (x == 0 && y == 1)
                dir = Direction::up;
            else if (x == 0 && y == -1)
                dir = Direction::down;
            else if (x == 1 && y == 0)
                dir = Direction::right;
            else if (x == -1 && y == 0)
                dir = Direction::left;
            else if (x == 1 && y == 1)
                dir = Direction::upRight;
            else if (x == -1 && y == 1)
                dir = Direction::upLeft;
            else if (x == 1 && y == -1)
                dir = Direction::downRight;
            else // (x == -1 && y == -1)
                dir = Direction::downLeft;
        }
        else
            dir = Direction::none;
    }
    return dir;
}

string dirToStr (Direction dir) {
    string s;

    switch (dir) {
        case Direction::up:
            s = "U";  break;
        case Direction::down:
            s = "D";  break;
        case Direction::left:
            s = "L";  break;
        case Direction::right:
            s = "R";  break;
        case Direction::upRight:
            s = "UR"; break;
        case Direction::upLeft:
            s = "UL"; break;
        case Direction::downLeft:
            s = "DL"; break;
        case Direction::downRight:
            s = "DR"; break;
        default:
            s = "NONE"; 
    }
    return s;
}

int applyMovement (vector<vector<CellState>> & board, int startRow, int startCol, Direction dir) {
    int r = RETURN_FAILURE;

    // up/down movements are invalid for triangular board 
    // diagonal movements are invalid for all the boards except triangular board 
    if (isProperDirection(board, dir)) {
        // Check if movement is exceed board
        if (isMovable(board, startRow, startCol, dir)) {
            int jumpRow, jumpCol, targetRow, targetCol;
            getMoveCell(board, startRow, startCol, dir, jumpRow, jumpCol, targetRow, targetCol);

            // Apply movement
            board[startRow][startCol] = CellState::empty;
            board[jumpRow][jumpCol] = CellState::empty;
            board[targetRow][targetCol] = CellState::peg;
            r = RETURN_SUCCESS;
            showNextPageEffect();
        }
    }
    return r;
}

int getMoveCell (const vector<vector<CellState>> & board, int startRow, int startCol, Direction dir, int & jumpRow, int & jumpCol, int & targetRow, int & targetCol) {
    int r = RETURN_SUCCESS;

    // Pre condition for move... functions
    jumpRow = targetRow = startRow;
    jumpCol = targetCol = startCol;

    /*  For triangular board upRight and it's opposite downLeft are interpreted
        differently since represantation of triangular board. Look the 
        showTriangularBoard function comments to see more explanation.          */
    switch (dir) {
        case Direction::up:
            moveUp(jumpRow, targetRow);    break;
        case Direction::down:
            moveDown(jumpRow, targetRow);  break;
        case Direction::left:
            moveLeft(jumpCol, targetCol);  break;
        case Direction::right:
            moveRight(jumpCol, targetCol); break;
        case Direction::upRight:
            if (isTriangularBoard(board) == false)
                moveRight(jumpCol, targetCol); 
            moveUp(jumpRow, targetRow);    break;
        case Direction::upLeft:
            moveUp(jumpRow, targetRow);
            moveLeft(jumpCol, targetCol);  break;
        case Direction::downRight:
            moveDown(jumpRow, targetRow);  
            moveRight(jumpCol, targetCol); break;
        case Direction::downLeft:
            if (isTriangularBoard(board) == false)
                moveLeft(jumpCol, targetCol);
            moveDown(jumpRow, targetRow); break;
        default:
            r = RETURN_FAILURE;
    }

    return r;
}

void moveUp (int & jumpRow, int & targetRow) {
    --jumpRow;
    targetRow -= 2;
}

void moveDown (int & jumpRow, int & targetRow) {
    ++jumpRow;
    targetRow += 2;
}

void moveRight (int & jumpCol, int & targetCol) {
    ++jumpCol;    
    targetCol += 2;    
}

void moveLeft (int & jumpCol, int & targetCol) {
    --jumpCol;    
    targetCol -= 2;    
}

bool isMovable (const vector<vector<CellState>> & board, int startRow, int startCol) {
    bool r;
    if (isTriangularBoard(board))
        // Try all the directions except up and down, since they are invalid/ambiguous
        r = isMovable(board, startRow, startCol, Direction::upLeft)    ||
            isMovable(board, startRow, startCol, Direction::upRight)   ||
            isMovable(board, startRow, startCol, Direction::downLeft)  ||
            isMovable(board, startRow, startCol, Direction::downRight) ||
            isMovable(board, startRow, startCol, Direction::left)      ||
            isMovable(board, startRow, startCol, Direction::right);
    else 
        // Try all the directions except diagonal ones, since they are invalid
        r = isMovable(board, startRow, startCol, Direction::up)   ||
            isMovable(board, startRow, startCol, Direction::down) ||
            isMovable(board, startRow, startCol, Direction::left) ||
            isMovable(board, startRow, startCol, Direction::right);
    return r;
}

bool isMovable (const vector<vector<CellState>> & board, int startRow, int startCol, Direction dir) {
    int r; 
    
    if (isProperDirection(board, dir)) {
        int v, jumpRow, jumpCol, targetRow, targetCol;
        v = getMoveCell(board, startRow, startCol, dir, jumpRow, jumpCol, targetRow, targetCol);
        
        r = v == RETURN_SUCCESS                             &&
            isInBoard(board, startRow, startCol)            && 
            board[startRow][startCol] == CellState::peg     &&
            isInBoard(board, jumpRow, jumpCol)              && 
            board[jumpRow][jumpCol] == CellState::peg       &&
            isInBoard(board, targetRow, targetCol)          && 
            board[targetRow][targetCol] == CellState::empty;
    }
    else
        r = false; 
    return r;
}

bool isProperDirection (const vector<vector<CellState>> & board, Direction dir) {
    bool r;

    // up/down movements are invalid for triangular board 
    // diagonal movements are invalid for non-triangular board 
    if (dir != Direction::none) {
        bool triangularBoard = isTriangularBoard(board);
        r =  triangularBoard  &&  isTriangularMovement(dir) ||
            !triangularBoard  && !isDiagonalMovement(dir); 
    }
    else 
        r = false;
    return r;
}

bool isTriangularMovement (Direction dir) {
    return  dir == Direction::right     || dir == Direction::left      ||
            dir == Direction::upRight   || dir == Direction::upLeft    ||
            dir == Direction::downRight || dir == Direction::downLeft;
}

bool isDiagonalMovement (Direction dir) {
    return dir == Direction::upRight || dir == Direction::upLeft || dir == Direction::downRight || dir == Direction::downLeft;
}

bool isGameOver (const vector<vector<CellState>> & board) {
    bool r = true;
    
    // The game continues until there are no pegs to move legally.
    for (int i = 0; i < board.size() && r == true; ++i)
        for (int j = 0; j < board[i].size() && r == true; ++j)
            if (board[i][j] == CellState::peg)
                r = !isMovable(board, i, j);

    return r;
}

int calculateScore (const vector<vector<CellState>> & board) {
    int score = 0;

    for (int i = 0; i < board.size(); ++i) 
        for (auto it = board[i].begin(); it != board[i].end(); ++it)
            if (*it == CellState::peg)
                ++score;

    return score;
}

void showGameRules () {

    cout << "================ GAME RULES ================\n\n"
         << "Movement Directions\n"
         << "---------------------------------------------\n"
         << " U: Up                 U\n"
         << " D: Down           UL  |  UR\n"
         << " L: Left        L -----|----- R\n"
         << " R: Right          DL  |  DR\n"
         << "                       D\n\n";
    cout << "Triangular Board\n"
         << "---------------------------------------------\n"
         << "Interface                  Notation\n" 
         << "------------------         ------------------ \n"  
         << "         A\n"
         << "          B\n"
         << "1      .   C                       A1\n"
         << "2     P P   D                    A2  B2\n"
         << "3    P P P   E                 A3  B3  C3\n"
         << "4   P P P P                  A4  B4  C4  D4\n"
         << "5  P P P P P               A5  B5  C5  D5  E5\n\n"
         << "// U and D movements are invalid for triangular board\n\n";

    cout << "Non-Triangular Board\n"
         << "---------------------------------------------\n"
         << "Interface                  Notation\n" 
         << "------------------         ------------------\n"  
         << "   A B C D E F G\n\n"
         << "1      P P P                     C1 D1 E1\n"
         << "2    P P P P P                B2 C2 D2 E2 F2\n"
         << "3  P P P . P P P           A3 B3 C3 D3 E3 F3 G3\n"
         << "4  P P P P P P P           A4 B4 C4 D4 E4 F4 G4\n"
         << "5  P P P P P P P           A5 B5 C5 D5 E5 F5 G5\n"
         << "6    P P P P P                B6 C6 D6 E6 F6\n"
         << "7      P P P                     C7 D7 E7\n\n"
         << "// Diagonal movements(UL, DL, UR, DR) are invalid for non-triangular board\n\n";

    cout << "================ GAME MODES ================\n\n"
         << "// There are 2 type of games: Human mode and Computer mode\n"
         << "Human Mode\n"
         << "------------------\n"
         << "// All the movements done by the player\n"
         << "Computer Mode\n"
         << "------------------\n"
         << "// All the movements done by the computer\n\n";

    cout << "============= EXAMPLE MOVEMENTS ============\n\n"
         << "Diagonal Movements\n"
         << "------------------\n"
         << "A3-UR: Select cell at coloumn A, row 3 and move to the Up-Right\n"
         << "B3-DL: Select cell at coloumn B, row 3 and move to the Down-Left\n\n"
         << "Direct Movements:\n"
         << "------------------\n"
         << "C6-U : Select cell at coloumn C, row 6 and move to the Up\n"
         << "E5-R : Select cell at coloumn E, row 5 and move to the Right\n\n"
         << "EXIT Command:\n"
         << "------------------\n"
         << "// Type EXIT to exit the movement screen\n\n";
    
    cout << "================= COMMANDS ================\n\n"
         << "/****/ You can use these commands when you see terminal sign(>>)\n\n"
         << "// exit: Exits from current place\n"
         << "       usage: exit\n\n"
         << "// save: Saves the current progress of the game\n"
         << "       usage: save filename.txt\n\n"
         << "// load: Loads the spefic game\n"
         << "       usage: load filename.txt\n\n";

    cout << "Enter to continue ";
    cin.get();
}

void showGameResult (int score, int numberOfMovement) {
    cout << "\n============== GAME IS OVER ==============\n" 
         << "\tNumber of Movement: " << numberOfMovement << endl
         << "\tScore: " << score << "\n"
         << "==========================================\n\n";
}

void welcomeGreet () {
    cout << "=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=\n"
         << "|            WELCOME TO PEG SOLITAIRE             |\n"
         << "=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=\n\n";
}

/***********************************************************************************
 * Board Start
 **********************************************************************************/

void initBoard (vector<vector<CellState>>& b, BoardType btype) {
    switch (btype) {
        case BoardType::french:
            initBoardFrench(b);         break;
        case BoardType::german:
            initBoardGerman(b);         break;
        case BoardType::asymmetrical:
            initBoardAsymmetrical(b);   break;
        case BoardType::english:
            initBoardEnglish(b);        break;
        case BoardType::diamond:
            initBoardDiamond(b);        break;
        case BoardType::triangular:
            initBoardTriangular(b);     break;
        default:
           cerr << "(!) Undefined board type. Board was unable to create correctly\n";
    }
}

void initBoardFrench(vector<vector<CellState>> & board) {
    createBoard(board, 7, 7, CellState::peg);
    for (int i = 0, n = 2; i < 2; ++i, --n)
        for (int j = 0; j < n; ++j)
            board[i][j] = CellState::out;

    for (int i = 0, n = 5; i < 2; ++i, ++n)
        for (int j = n; j < 7; ++j)
            board[i][j] = CellState::out;

    for (int i = 5, n = 1; i < 7; ++i, ++n)
        for (int j = 0; j < n; ++j)
            board[i][j] = CellState::out;

    for (int i = 5, n = 6; i < 7; ++i, --n)
        for (int j = n; j < 7; ++j)
            board[i][j] = CellState::out;
    
    board[2][3] = CellState::empty;
}   

void initBoardGerman(vector<vector<CellState>> & board) {
    createBoard(board, 9, 9, CellState::peg);
    for (int i = 0; i < 3; ++i)
        for (int j = 0; j < 3; ++j)
            board[i][j] = CellState::out;

    for (int i = 0; i < 3; ++i)
        for (int j = 6; j < 9; ++j)
            board[i][j] = CellState::out;

    for (int i = 6; i < 9; ++i)
        for (int j = 0; j < 3; ++j)
            board[i][j] = CellState::out;

    for (int i = 6; i < 9; ++i)
        for (int j = 6; j < 9; ++j)
            board[i][j] = CellState::out;
    
    board[4][4] = CellState::empty;
}   

void initBoardAsymmetrical(vector<vector<CellState>> & board) {
    createBoard(board, 8, 8, CellState::peg);
    for (int i = 0; i < 3; ++i)
        for (int j = 0; j < 2; ++j)
            board[i][j] = CellState::out;

    for (int i = 0; i < 3; ++i)
        for (int j = 5; j < 8; ++j)
            board[i][j] = CellState::out;

    for (int i = 6; i < 8; ++i)
        for (int j = 0; j < 2; ++j)
            board[i][j] = CellState::out;

    for (int i = 6; i < 8; ++i)
        for (int j = 5; j < 8; ++j)
            board[i][j] = CellState::out;
    
    board[4][3] = CellState::empty;
}  

void initBoardEnglish(vector<vector<CellState>> & board) {
    createBoard(board, 7, 7, CellState::peg);
    for (int i = 0; i < 2; ++i)
        for (int j = 0; j < 2; ++j)
            board[i][j] = CellState::out;

    for (int i = 0; i < 2; ++i)
        for (int j = 5; j < 7; ++j)
            board[i][j] = CellState::out;

    for (int i = 5; i < 7; ++i)
        for (int j = 0; j < 2; ++j)
            board[i][j] = CellState::out;

    for (int i = 5; i < 7; ++i)
        for (int j = 5; j < 7; ++j)
            board[i][j] = CellState::out;
    board[3][3] = CellState::empty;
}   

void initBoardDiamond(vector<vector<CellState>> & board) {
    createBoard(board, 9, 9, CellState::peg);

    for (int i = 0, n = 4; i < 4; ++i, --n)
        for (int j = 0; j < n; ++j)
            board[i][j] = CellState::out;

    for (int i = 0, n = 5; i < 4; ++i, ++n)
        for (int j = n; j < 9; ++j)
            board[i][j] = CellState::out;

    for (int i = 5, n = 1; i < 9; ++i, ++n)
        for (int j = 0; j < n; ++j)
            board[i][j] = CellState::out;

    for (int i = 5, n = 8; i < 9; ++i, --n)
        for (int j = n; j < 9; ++j)
            board[i][j] = CellState::out;
    board[4][4] = CellState::empty;
}   

void initBoardTriangular(vector<vector<CellState>> & board) {
    board.resize(5);
    for (int i = 0; i < 5; ++i) {
        board[i].resize(i + 1);
        for (int j = 0; j <= i; ++j)
            board[i][j] = CellState::peg;
    }
    board[0][0] = CellState::empty;
}   

void createBoard (vector<vector<CellState>> & b, int row, int col, CellState c) {
    b.resize(row);
    for (int i = 0; i < row; ++i) {
        b[i].resize(col);
        for (int j = 0; j < col; ++j)
            b[i][j] = c;
    }
}

void showGameStatus (const vector<vector<CellState>> & board, int numberOfMovement, GameMode gm) {
    // Playing game mode could be Computer or Human 
    cout << "\n-----------------------------\n"
         << "Game Mode: " << (gm == GameMode::computer ? "Computer" : "Human") << endl
         << "Number of Moves: " << numberOfMovement << endl
         << "-----------------------------\n";
    showBoard(board);
}

void showBoard (const vector<vector<CellState>> & b) {
    if (isTriangularBoard(b))
        showTriangularBoard(b);
    else 
        showNonTriangularBoard(b);
}


/**
 * @brief: Triangular Board Representation 
 * 
 *    User Interface          User Notation            Array Representation
 *    ----------------------  ----------------------   ----------------------
 *             A                                       
 *              B  
 *    1      .   C                     A1               A1
 *    2     P P   D                  A2  B2             A2 B2  
 *    3    P P P   E               A3  B3  C3           A3 B3 C3
 *    4   P P P P                A4  B4  C4  D4         A4 B4 C4 D4
 *    5  P P P P P             A5  B5  C5  D5  E5       A5 B5 C5 D5 E5        
 */

void showTriangularBoard (const vector<vector<CellState>> & b) {
    int padding = 3; 

    cout << endl;
    printn(' ', padding * 2 + b.size());
    cout << "A\n";
    printn(' ', padding * 2 + b.size() + 1);
    cout << "B\n";

    for (int i = 0; i < b.size(); ++i) {
        int bottomLen = b[b.size() - 1].size();
        
        cout << 1 + i;
        printn(' ', padding + bottomLen - i - 1);

        for (int j = 0, n = i + 1; j < n; ++j)
            cout << cellStateToChar(b[i][j]) << ' ';

        if (i < b.size() - 2) {
            printn(' ', padding);
            cout << static_cast<char>('C' + i);
        }
        cout << endl;
    }
}

void showNonTriangularBoard (const vector<vector<CellState>> & b) {
    int padding = 3;
    int colMax = 0;

    // Define the max column number
    for (int i = 0; i < b.size(); ++i)
        if (b[i].size() > colMax)
            colMax = b[i].size();

    // Print column order as letter
    cout << endl;
    printn(' ', padding + 1);
    for (int j = 0; j < colMax; ++j)
        cout << static_cast<char>('A' + j) << ' ';
    cout << "\n\n";

    for (int i = 0; i < b.size(); ++i) {
        // Print the row order as number
        cout << 1 + i;
        printn(' ', padding);
        for (auto it = b[i].begin(); it != b[i].end(); ++it)
            cout << cellStateToChar(*it) << ' ';
        cout << endl;
    }
}

void printn (char c, int n) {
    for (int i = 0; i < n; ++i)
        cout << c;
}

char cellStateToChar (CellState cs) {
    char c;

    switch (cs) {
        case CellState::peg:
            c = 'P';
            break;
        case CellState::empty:
            c = '.';
            break;
        case CellState::out:
            c = ' ';
            break;
        default:
            c = '\0';
            break;
    }
    return c;
}

bool isInBoard (const vector<vector<CellState>> & b, int row, int col) {
    return      0 <= row && row < b.size()      &&
                0 <= col && col < b[row].size() &&
                b[row][col] != CellState::out;
}

bool isTriangularBoard (const vector<vector<CellState>> & b) {
    bool r = true;

    for (int i = 0; i < b.size() && r == true; ++i)
        r = b[i].size() == i + 1;
    return r;
}

void printAllBoardTypes () {
    cout    << "1- French                2- German\n"
            << "--------------------     --------------------\n"
            << "      P P P                      P P P\n"
            << "    P P P P P                    P P P\n"
            << "  P P P . P P P                  P P P\n"
            << "  P P P P P P P            P P P P P P P P P\n"
            << "  P P P P P P P            P P P P . P P P P\n"
            << "    P P P P P              P P P P P P P P P\n"
            << "      P P P                      P P P\n"
            << "                                 P P P\n"
            << "                                 P P P\n"
            << "\n";

    cout    << "3- Asymmetrical          4- English\n"
            << "--------------------     --------------------\n"
            << "      P P P                    P P P\n"
            << "      P P P                    P P P\n"
            << "      P P P                P P P P P P P\n"
            << "  P P P P P P P P          P P P . P P P\n"
            << "  P P P . P P P P          P P P P P P P\n"
            << "  P P P P P P P P              P P P\n"
            << "      P P P                    P P P\n"
            << "      P P P\n"
            << "\n";
    
    cout    << "5- Diamond               6- Triangle\n"
            << "--------------------     --------------------\n"
            << "          P                    .\n"
            << "        P P P                 P P\n"
            << "      P P P P P              P P P\n"
            << "    P P P P P P P           P P P P\n"
            << "  P P P P . P P P P        P P P P P\n"
            << "    P P P P P P P\n"
            << "      P P P P P\n"
            << "        P P P\n"
            << "          P\n"
            << "\n\n";
}

/***********************************************************************************
 * Load(Import) & Save(Export)  
 **********************************************************************************/
int saveGame (const vector<vector<CellState>> & board, const char * fileName, GameMode gm, int numberOfMovement) {
    ofstream outStream(fileName, ofstream::out);
    int r;

    if (! outStream.fail()) {
        outStream << numberOfMovement << ' ' << gameModeToChar(gm) << endl;
        exportBoard(board, outStream);
        outStream.close();
        r = RETURN_SUCCESS;
    }
    else {
        cerr << "(!) File was unable to open correctly\n"; 
        r = RETURN_FAILURE; 
    }

    return r;
}

int loadGame (vector<vector<CellState>> & board, const char * fileName, GameMode & gm, int & numberOfMovement) {
    ifstream inStream(fileName, ifstream::in);
    int r = RETURN_FAILURE, n;
    GameMode gm_load; 
    string s;

    if (! inStream.fail()) {
        // Get the first line of file
        inStream >> n >> s;
        inStream.get();     // get rid of new line charachter('\n)'
        gm_load = charToGameMode(s[0]);

        // Check the file configuration
        if (gm_load != GameMode::none && (gm == GameMode::undefined || gm_load == gm)) {
            r = importBoard(board, inStream);        
            if (r == RETURN_SUCCESS) {
                numberOfMovement = n;
                gm = gm_load;
            }
            else 
                cerr << "(!) Invalid board configuration. Game was unable to load correctly\n";
        }
        else
            cerr << "(!) Invalid game mode. Game was unable to load correctly\n"; 
        inStream.close();
    }
    else
        cerr << "(!) File was unable to open correctly\n"; 

    return r;
}

int importBoard (vector<vector<CellState>> & b, ifstream & inStream) {
    int r;

    if (! inStream.fail()) {
        int i = 0, colCur = 0, colMax = 0;
        r = RETURN_SUCCESS;
        char c1, c2;

        b.resize(0);    // Clear the vector
        b.resize(1);

        //! Board should have max 26 col and 9 row for good GUI 
        while (inStream.get(c1) && r == RETURN_SUCCESS) {
            inStream.get(c2);
            
            switch (c1) {
                case 'P':
                    b[i].push_back(CellState::peg);     break;
                case ' ':
                    b[i].push_back(CellState::out);     break;
                case '.':
                    b[i].push_back(CellState::empty);   break;
                default:
                    cerr << "(!) Invalid charachter " << '"' << c1 << '"' << endl;
                    r = RETURN_FAILURE;
            }

            // Create a new row
            if (r == RETURN_SUCCESS && c2 == '\n') {
                ++i;
                b.resize(b.size() + 1);
                // Keep the Max coloumn number to have a rectangular 2D vector
                if (colCur > colMax)
                    colMax = colCur;
            }
        }
    }
    else
        r = RETURN_FAILURE;
    return r;
}

void exportBoard (const vector<vector<CellState>> & b, ofstream & outStream) {
    for (int i = 0; i < b.size(); ++i) {
        for (auto it = b[i].begin(); it != b[i].end(); ++it) {
            outStream << cellStateToChar(*it);
            if (it + 1 != b[i].end()) 
                outStream << ' ';
        }
        if (i + 1 != b.size()) 
            outStream << endl;
    }
}

Command whichCommand (const string & s) {
    Command c = Command::none;

    if (s.length() >= 4) {
        if (upperCase(s[0]) == 'E' && upperCase(s[1]) == 'X' &&
            upperCase(s[2]) == 'I' && upperCase(s[3]) == 'T' )
            c = Command::exit;
        else if (upperCase(s[0]) == 'L' && upperCase(s[1]) == 'O' &&
                 upperCase(s[2]) == 'A' && upperCase(s[3]) == 'D' )
            c = Command::load;
        else if (upperCase(s[0]) == 'S' && upperCase(s[1]) == 'A' &&
                 upperCase(s[2]) == 'V' && upperCase(s[3]) == 'E' )
            c = Command::save;
    }
    return c;
}

Command whichCommand (const char * s) {
    Command c = Command::none;

    if (cstrlen(s) >= 4) {
        if (upperCase(s[0]) == 'E' && upperCase(s[1]) == 'X' &&
            upperCase(s[2]) == 'I' && upperCase(s[3]) == 'T' )
            c = Command::exit;
        else if (upperCase(s[0]) == 'L' && upperCase(s[1]) == 'O' &&
                 upperCase(s[2]) == 'A' && upperCase(s[3]) == 'D' )
            c = Command::load;
        else if (upperCase(s[0]) == 'S' && upperCase(s[1]) == 'A' &&
                 upperCase(s[2]) == 'V' && upperCase(s[3]) == 'E' )
            c = Command::save;
    }
    return c;
}

/***********************************************************************************
 * Utility 
 **********************************************************************************/

bool getChoice (string prompt) {
    bool exit = false;
    string s;
    char c;

    cout << prompt;
    do {
        cin >> s;
        c = upperCase(s[0]);
        
        if (c == 'Y' || c == 'N')
            exit = true;
        else
            cout << "Please select a proper choose: ";
    } while (exit == false);
    cin.get();  // consume \n
    return c == 'Y' ? true : false;
}

int getChoice (string prompt, int lb, int ub) {
    bool exit = false;
    string s;
    int r;
    
    cout << prompt;    
    do {
        cin >> s;
        r = strToInt(s);

        if (r != RETURN_FAILURE && isInRange(r, lb, ub))
            exit = true;
        else
            cout << "Please select a proper choose: ";
    } while (exit == false);
    cin.get();  // consume \n
    return r;    
}

int getChoice (string inPrompt, string errPrompt, int lb, int ub) {
    bool exit = false;
    string s;
    int r;
    
    cout << inPrompt;    
    do {
        cin >> s;
        r = strToInt(s);

        if (r != RETURN_FAILURE && isInRange(r, lb, ub))
            exit = true;
        else
            cout << errPrompt << endl
                 << "Please select a proper choose: ";
    } while (exit == false);
    cin.get();  // consume \n
    return r;
}

string BoardTypeToStr (BoardType btype) {
    string s;
    switch (btype) {
        case BoardType::french:
            s = "French";       break;
        case BoardType::german:
            s = "German";       break;
        case BoardType::asymmetrical:
            s = "Asymmetrical"; break;
        case BoardType::english:
            s = "English";      break;
        case BoardType::diamond:
            s = "Diamond";      break;
        case BoardType::triangular:
            s = "Triangular";   break;
        default:
            cerr << "(!) Undefined board type";
    }
    return s;
}

int strToInt (string & s) {
    int r = 0;

    // RETURN_FAILURE is negative constant value
    for (int i = 0; i < s.length() && r != RETURN_FAILURE; ++i)
        if (isDigit(s[i]))
            r = r * 10 + s[i] - '0';
        else 
            r = RETURN_FAILURE;

    return r;
}

char gameModeToChar (GameMode gm) {
    char r;
    switch (gm) {
        case GameMode::computer:
            r = 'C'; break;
        case GameMode::human:
            r = 'H'; break;
        case GameMode::undefined:
            r = 'U'; break;
        case GameMode::none:
            r = 'N';   
    }
    return r;
}

GameMode charToGameMode (char gm) {
    GameMode r;
    switch (upperCase(gm)) {
        case 'C':
            r = GameMode::computer;  break;
        case 'H':
            r = GameMode::human;     break;
        case 'U':
            r = GameMode::undefined; break;
        case 'N':
        default:
            r = GameMode::none;
    }

    return r;
}

void strToCstr (const string & str, char * cstr) {
    int i;
    for (i = 0; i < str.length(); ++i)
        cstr[i] = str[i];
    cstr[i] = '\0';
}

int cstrlen (const char * s) {
    int r = 0;
    if (s != NULL)
        while (s[r] != '\0')
            ++r;
    return r;
}

void showNextPageEffect () {
    cout << "\n>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>\n\n";
}

bool isInRange (int n, int lb, int ub) {
    return lb <= n && n <= ub;
}

bool isDigit (char c) {
    return '0' <= c && c <= '9';
}

bool isLetter (char c) {
    c = upperCase(c);
    return 'A' <= c && c <= 'Z';
}

char upperCase (char c) {
    return 'a' <= c && c <= 'z' ? 'A' + c - 'a' : c;
}

void convertUpperCase (string & s) {
    for (auto it = s.begin(); it != s.end(); ++it)
        *it = upperCase(*it); 
}