#include "pegSolitaire.h"

/***********************************************************************************
 *                                  PegSolitaire
 **********************************************************************************/

PegSolitaire::PegSolitaire (const string & filename) {
    string command(filename);
    bool exit = false;
    
    while (load(command.c_str()) == RETURN_FAILURE && !exit) {
        cerr << "(!) File cannot loaded in a proper way\n";
        cout << "Enter the file name: ";
        cin  >> command;
        if (strUp(command) == "EXIT")
            exit = true;
    }
    cin.get();          // Get newline-charachter from stream
    if (!exit) {
        ++gameNumber;   // New game added
        startGame();    
    }
    else
        setGameActivity(false);
}

PegSolitaire::PegSolitaire (int boardType) {
    setBoard(boardType);    // Sets the game board and all the game configuration    
    if (getGameActivity()) {
        ++gameNumber;           // New game added
        startGame();
    }
}

PegSolitaire::PegSolitaire () {
    gameMode = GameMode::undefined;
    setGameConfig();    // Set game config for undefined board  
    ++gameNumber;
    startGame();
}

inline int PegSolitaire::getBoardRow () const {return boardRow;}

inline int PegSolitaire::getBoardCol () const {return boardCol;}

inline PegSolitaire::CellValue PegSolitaire::getCellValue (int row, int col) const {
    if (0 <= row && row <= getBoardRow() && 0 <= col && col <= getBoardCol())
        return board[row][col].getValue();
    else {
        cerr << "(!) Access to cell[" << row << "][" << col << "] is denied\n"; 
        return CellValue::undefined;
    }
}

const PegSolitaire::Cell & PegSolitaire::getCell (int row, int col) const {
    if (0 <= row && row <= getBoardRow() && 0 <= col && col <= getBoardCol())
        return board[row][col];
    else {
        cerr << "(!) Access to cell[" << row << "][" << col << "] is denied\n"; 
        return board[0][0]; //!!!!!!!!!!!
    }
}

inline const vector<vector<PegSolitaire::Cell>> & PegSolitaire::getBoard () const {
    return board;
}

void PegSolitaire::startGame () {
    int choose, r;
    Movement mov;
    string command;
    bool exit = false;
    
    // check the game mode is defined
    if (gameMode == GameMode::undefined) {
        cout << "GAME MODE MENU\n"
             << "1) Human Mode\n"
             << "2) Computer Mode\n"
             << "0) Go Back to MAIN MENU\n";
        choose = getChoice("Choose an option: ", 0, 2);
        if (choose == 1)
            gameMode = GameMode::human;
        else if (choose == 2)
            gameMode = GameMode::computer;
    }

    //! better design needed+
    switch (gameMode) {
        case GameMode::human:
            // Set board if it is not initialized yet
            if (getBoardRow() == 0)    setBoard();
            displayGame();
            while (!isGameOver() && !exit) {
                cout << "\nMovement: ";
                cin >> command;
                mov.setMovement(command);
                if (applyMovement(mov) == RETURN_SUCCESS)
                    displayGame();
                else {
                    r = applyCommand(command);
                    if (r == RETURN_EXIT)
                        exit = true;
                    else if (r == RETURN_FAILURE)
                        cerr << "(!) Undefined command\n";  
                }
            }
            // if an active game played show the result, o.w just exit
            if (getGameActivity()) {
                displayGameStatus();
                cout << "\nGAME IS OVER. "; 
                enterTo("Enter to continue ");
            }
            break;
        case GameMode::computer:
            playGame();
            break;
        case GameMode::undefined:
            cout << "EXIT\n";
    }
}

PegSolitaire::Movement PegSolitaire::play () {
    int rowBound = getBoardRow();
    int colBound = getBoardCol();
    Movement mov; // In case of there is no move left
    srand(time(NULL));
    int iCount, jCount;

    // Selects a random starting point and applies the first valid movement
    // If there is no movement left then returns no-parameter constructor value
    // which can be detected by (mov.getDirection() == Direction::undefined)
    for (int i = rand() % rowBound, iCount = 0; iCount < rowBound; ++iCount) {
        for (int j = rand() % colBound, jCount = 0; jCount < colBound; ++jCount) {
            if (board[i][j].getValue() == CellValue::peg) {
                if (isValidMovement(i, j, Direction::up))
                    mov = Movement(i, j, Direction::up);
                else if (isValidMovement(i, j, Direction::down))
                    mov = Movement(i, j, Direction::down);
                else if (isValidMovement(i, j, Direction::right))
                    mov = Movement(i, j, Direction::right);
                else if (isValidMovement(i, j, Direction::left))
                    mov = Movement(i, j, Direction::left);
            }
            if (j + 1 == colBound) j = 0;
            else ++j;
        }
        if (i + 1 == rowBound) i = 0;
        else ++i;
    }

    if (iCount != rowBound || jCount != colBound)   // Game is not over
        applyMovement(mov);

    return mov; 
}

void PegSolitaire::play (const Movement & mov) {
    applyMovement(mov);
}

void PegSolitaire::playGame () { 
    int r; 
    // Set the board if it's not defined yet
    if (boardRow == 0)
        r = setBoard();
    else
        r = RETURN_SUCCESS;

    // Plays the game with random movements
    if (r != RETURN_EXIT) {
        Movement mov;  // undefined-direction means no movement left

        displayGame();
        cout << "\n# You can use command (load, save exit) or continue playing by entering.\n"
             << "Enter to start ";
        cin.get();

        for (mov = play(); r != RETURN_EXIT && mov.getDirection() != Direction::undefined; mov = play()) {
            // Show the movement that made and take a command
            displayGame();
            cout << "\nApplied Movement: " << mov << " ";

            string command;
            char c;
            for (cin.get(c); c != '\n' && c != ' '; cin.get(c))
                command.push_back(c);
            
            if (command.length() > 0) {
                r = applyCommand(command);
                if (r == RETURN_FAILURE)
                    cerr << "\n(!) Undefined command\n";
            }
        } 
    }

    // if an active game played show the result, o.w just exit
    if (getGameActivity()) {
        displayGameStatus();
        cout << "\nGAME IS OVER. ";
        enterTo("Enter to continue ");
    }
}

int PegSolitaire::applyMovement (const Movement & mov) {
    Cell & start = board[mov.getStartRow()][mov.getStartCol()];
    Cell & jump = jumpCell(mov);
    Cell & target = targetCell(mov);
    
    if (isValidMovement(start, jump, target)) {
        start.setValue(CellValue::empty);
        jump.setValue(CellValue::empty);
        target.setValue(CellValue::peg);
        ++numberOfMovement;
        --score;
        --pegNumber;    // Decrease the total number of peg
        return RETURN_SUCCESS;
    }
    else
        return RETURN_FAILURE;
}

int PegSolitaire::applyCommand (const string & command) {
    int r = RETURN_SUCCESS;
    bool choice;
    string cmd = strUp(command);
    char filename[1000];
    char end;  // To detect space and newline charachters 

    switch (whichCommand(cmd)) {
        case Command::load:
            cin >> filename;
            cin.get(end);
            load(filename);
            r = RETURN_SUCCESS;
            break;
        case Command::save:
            do {
                cin >> filename;
                cin.get(end);
                r = save(filename);
                if (r == RETURN_SUCCESS)
                    cout << "\nGame saved succesfully (filename: " << filename << ")\n";
                else
                    cout << "\nGame cannot saved properly (filename: " << filename << ")\n";   
            } while (end != '\n');
            
            choice = getChoice("Do you want to continue to play (y or n) ");
            r = choice == true ? RETURN_SUCCESS : RETURN_EXIT;
            break;
        case Command::exit:
            r = RETURN_EXIT;
            break;
        case Command::undefined:
            r = RETURN_FAILURE;
    }
    return r;
}

int PegSolitaire::load (const char * fileName) {
    ifstream inStream(fileName);
    int r = RETURN_FAILURE;
    
    if (!inStream.fail()) {
        // import game the configuration
        char gm;
        int nom;
        inStream >> gm >> nom;
        inStream.get(); // get the newline charachter from stream
        
        if ((gm == 'C' || gm == 'H') && nom >= 0) {
            gameMode = static_cast<GameMode>(charUp(gm));
            numberOfMovement = nom;
            r = RETURN_SUCCESS;
            board.clear();
            board.resize(1);

            char c1, c2;
            int i = 0, maxCol = 0;
            // import the game board
            while (inStream.get(c1) && r == RETURN_SUCCESS) {
                inStream.get(c2);
                switch (c1) {
                    case 'P':
                        board[i].push_back(Cell(i, board[i].size(), CellValue::peg)); break;
                    case '.':
                        board[i].push_back(Cell(i, board[i].size(), CellValue::empty)); break;
                    case '#':
                        board[i].push_back(Cell(i, board[i].size(), CellValue::junction)); break;
                    case ' ':
                        board[i].push_back(Cell(i, board[i].size(), CellValue::wall)); break;
                    default:
                        cerr << "(!) Invalid charachter " << '"' << c1 << '"' << endl;
                        r = RETURN_FAILURE;
                }

                if (r == RETURN_SUCCESS) { 
                    if (c2 == '\n') {
                        // Update the max coloumn number according to current row
                        if (board[i].size() > maxCol)
                            maxCol = board[i].size();
                        // Increase the row number
                        board.resize(board.size() + 1);
                        ++i;     
                    }
                    else if (c2 != ' ') {
                        cerr << "(!) Invalid charachter " << '"' << c2 << '"' << endl;
                        r = RETURN_FAILURE;
                    } 
                } 
            } 
            // Set the game configuration for loaded board 
            if (r == RETURN_SUCCESS) 
                setGameConfig(i + 1, maxCol);
            else
                setGameActivity(false); // Inactive game 
        }        
    }
    return r;
}

int PegSolitaire::save (const char * fileName) const {
    ofstream outStream(fileName);
    int r;
    
    if (!outStream.fail()) {
        // export the game configuration  
        outStream << getGameMode() << ' ' << getNumberOfMov() << endl;
        // export the game board
        for (int i = 0; i < board.size(); ++i) {
            for (int j = 0; j < board[i].size(); ++j) {
                outStream << static_cast<char>(board[i][j].getValue());
                if (j + 1 < board[i].size())
                    outStream << ' ';
            }
            if (i + 1 < board.size())
                outStream << endl;
        }  
        outStream.close();
        r = RETURN_SUCCESS;
    }
    else
        r = RETURN_FAILURE;

    return r;
}

bool PegSolitaire::compare (const PegSolitaire & other) {
    // Score value is the number of remaining pegs
    return  getScore() > other.getScore();
}

inline int PegSolitaire::getNumberOfMov() const {return numberOfMovement;}

inline int PegSolitaire::getScore() const {return score;}

inline char PegSolitaire::getGameMode() const {return static_cast<char>(gameMode);}

int PegSolitaire::getPegNumber () {return pegNumber;}

int PegSolitaire::getActiveGameNumber () {return gameNumber;}

bool PegSolitaire::isGameOver () {
    bool over = true;
    for (int i = 0; i < board.size() && over == true; ++i)
        for (int j = 0; j < board[i].size() && over == true; ++j)
            over = !isMovable(board[i][j]);    
    return over;
}

inline bool PegSolitaire::getGameActivity () const {return activeGame;}

void PegSolitaire::setGameActivity (bool activity) {activeGame = activity;}

void PegSolitaire::displayGame () const {
    displayGameStatus();
    displayBoard();
}

void PegSolitaire::displayGameStatus () const {
    cout << "\n============== GAME STATUS ==============\n" 
         << "Game mode      : " << (gameMode == GameMode::human ? "Human" : "Computer") << endl 
         << "Number of moves: " << getNumberOfMov() << endl
         << "Score          : " << getScore() << "\n"
         << "==========================================\n\n";
}

void PegSolitaire::displayBoard () const {
    const int ENG_LETTER_NUM = 26;                  
    int padding = getBoardRow() / ENG_LETTER_NUM  + 1;   // Define the space between coloumns
    int margin = getBoardRow() / 10 + 3;

    // Print the coloumns
    string str_col = "A";
    cout << ('\n') << setw(str_col.size() + margin);

    for (int i = 0; i < boardCol; ++i) {
        // Print current coloumn and iterate next col (AB becomes AC)
        cout << str_col << setw(padding);

        ++str_col[str_col.size() - 1];
        /* Be sure str_col does not go outside from the alphabet
           A B C ... AA AB AC ... AAA AAB AAC ...   */
        
        for (int j = str_col.size() - 1; j >= 0 && str_col[j] > 'Z'; --j) {
            str_col[j] = 'A';

            if (j == 0)
                str_col.push_back('A');    // Add new letter
            else
                ++str_col[j - 1];
        }
    }
    cout << "\n\n"; // margin-down for letter coloumn

    for (int i = 0; i < boardRow; ++i) {
        cout << i << setw(str_col.size() + margin - 1);
        for (int j = 0; j < boardCol; ++j) {
            // Print '#' (CellValue::junction) as ' '
            char cv = static_cast<char>(board[i][j].getValue());
            cout << (cv == '#' ? ' ' : cv) << setw(padding);
        }
        cout << endl;
    }
}

PegSolitaire::Cell & PegSolitaire::jumpCell (const Movement & mov) {
    int startRow = mov.getStartRow(); 
    int startCol = mov.getStartCol(); 
    Direction dir = mov.getDirection();
    
    if (dir == Direction::up && startRow - 1 >= 0)
        return board[startRow - 1][startCol];            
    else if (dir == Direction::down && startRow + 1 < getBoardRow())
        return board[startRow + 1][startCol];            
    else if (dir == Direction::right && startCol + 1 < getBoardCol())
        return board[startRow][startCol + 1];            
    else if (dir == Direction::left && startCol - 1 >= 0)
        return board[startRow][startCol - 1];            
    else
        return board[startRow][startCol];
}

PegSolitaire::Cell & PegSolitaire::targetCell (const Movement & mov) {
    int startRow = mov.getStartRow(); 
    int startCol = mov.getStartCol(); 
    Direction dir = mov.getDirection();
    
    if (dir == Direction::up && startRow - 2 >= 0)
        return board[startRow - 2][startCol];            
    else if (dir == Direction::down && startRow + 2 < getBoardRow())
        return board[startRow + 2][startCol];            
    else if (dir == Direction::right && startCol + 2 < getBoardCol())
        return board[startRow][startCol + 2];            
    else if (dir == Direction::left && startCol - 2 >= 0)
        return board[startRow][startCol - 2];            
    else
        return board[startRow][startCol];
}

bool PegSolitaire::isMovable (const Cell & startCell) {
    return  isValidMovement(startCell.getRow(), startCell.getCol(), Direction::up)    ||
            isValidMovement(startCell.getRow(), startCell.getCol(), Direction::down)  ||
            isValidMovement(startCell.getRow(), startCell.getCol(), Direction::right) ||
            isValidMovement(startCell.getRow(), startCell.getCol(), Direction::left);
}

bool PegSolitaire::isValidMovement (int startRow, int startCol, Direction dir) {
    Cell & jump = jumpCell(Movement(startRow, startCol, dir));
    Cell & target = targetCell(Movement(startRow, startCol, dir));

    return (jump.getRow() != startRow || jump.getCol() != startCol) &&
            getCellValue(startRow, startCol) == CellValue::peg      &&
            jump.getValue() == CellValue::peg                       && 
            target.getValue() == CellValue::empty;
}

bool PegSolitaire::isValidMovement (const Movement & mov) {
    const Cell & start = board[mov.getStartRow()][mov.getStartCol()];   
    const Cell & jump = jumpCell(mov);
    const Cell & target = targetCell(mov);
    return isValidMovement(start, jump, target);
}

bool PegSolitaire::isValidMovement (const Cell & start, const Cell & jump, const Cell & target) {
    return (start.getRow() != target.getRow() || start.getCol() != target.getCol()) &&
            start.getValue() == CellValue::peg      &&
            jump.getValue() == CellValue::peg       && 
            target.getValue() == CellValue::empty;
}

void PegSolitaire::setGameConfig () {
    numberOfMovement = 0;      // Initialiaze the NOM                 
    if (gameMode == GameMode::undefined)
        // Initialize the board configuration as default values
        score = boardCol = boardRow = 0;
    else {
        setScore();    //! setscore can make score 0 when there is no board init yet?
        pegNumber += getScore();   // Increase the total pegNumber by the no. peg at new board
    }
}

void PegSolitaire::setGameConfig (int setBoardRow, int setBoardCol) {
    setScore();              
    pegNumber += getScore(); 
    boardRow = setBoardRow;  
    boardCol = setBoardCol;
    setGameActivity(true); 
    autoResize();            // Resize the board as it become NxN square
}


void PegSolitaire::setScore () {
    int count = 0;  // Number of peg at the game board
    for (int i = 0; i < board.size(); ++i)
        for (int j = 0; j < board[i].size(); ++j)
            if (board[i][j].getValue() == CellValue::peg)
                ++count;
    score = count;
}

int PegSolitaire::setBoard () {
    // Ask the board type and initialize the board
    displayAllBoardTypes();
    cout << "BOARD SELECTION MENU\n"
         << "1) French\n"
         << "2) German\n"
         << "3) Asymmetrical\n"
         << "4) English\n"
         << "5) Diamond\n"
         << "6) Triangle\n"
         << "7) Random Selection\n"
         << "0) Go Back to MAIN MENU\n";
    int choice = getChoice("Choose an option: ", 0, 7);
    
    // Select a random board
    if (choice == 0)
        return RETURN_EXIT;
    else {
        if (choice == 7) {
            srand(time(NULL));
            choice = rand() % 6 + 1;
            cout << endl << choice << " board selected randomly\n";
        }
        setBoard(choice);
        cout << endl;
        return RETURN_SUCCESS;
    }
}

void PegSolitaire::setBoard (int boardType) {
    switch (boardType) {
        case 1:
            setBoardFrench();         break;
        case 2:
            setBoardGerman();         break;
        case 3:
            setBoardAsymmetrical();   break;
        case 4:
            setBoardEnglish();        break;
        case 5:
            setBoardDiamond();        break;
        case 6:
            setBoardTriangular();     break;
        default:
            cerr << "(!) Undefined board type. Board was unable to create correctly\n";
            setGameActivity(false);
            return; //! BETTER DESIGN NEEDED
    }
    setGameConfig();
}

void PegSolitaire::setBoardFrench () {
    initBoard(7, 7, CellValue::peg);
    boardRow = boardCol = 7;

    for (int i = 0, n = 2; i < 2; ++i, --n)
        for (int j = 0; j < n; ++j)
            board[i][j].setValue(CellValue::wall);

    for (int i = 0, n = 5; i < 2; ++i, ++n)
        for (int j = n; j < 7; ++j)
            board[i][j].setValue(CellValue::wall);

    for (int i = 5, n = 1; i < 7; ++i, ++n)
        for (int j = 0; j < n; ++j)
            board[i][j].setValue(CellValue::wall);

    for (int i = 5, n = 6; i < 7; ++i, --n)
        for (int j = n; j < 7; ++j)
            board[i][j].setValue(CellValue::wall);
    
    board[2][3].setValue(CellValue::empty);
}   

void PegSolitaire::setBoardGerman () {
    initBoard(9, 9, CellValue::peg);
    boardRow = boardCol = 9;

    fillBoard(0, 3, 0, 3, CellValue::wall);
    fillBoard(0, 3, 6, 9, CellValue::wall);
    fillBoard(6, 9, 0, 3, CellValue::wall);
    fillBoard(6, 9, 6, 9, CellValue::wall);
    board[4][4].setValue(CellValue::empty);
}   

void PegSolitaire::setBoardAsymmetrical () {
    initBoard(8, 8, CellValue::peg);
    boardRow = boardCol = 8;

    fillBoard(0, 3, 0, 2, CellValue::wall);
    fillBoard(0, 3, 5, 8, CellValue::wall);
    fillBoard(6, 8, 0, 2, CellValue::wall);
    fillBoard(6, 8, 5, 8, CellValue::wall);
    board[4][3].setValue(CellValue::empty);
}  

void PegSolitaire::setBoardEnglish () {
    initBoard(7, 7, CellValue::peg);
    boardRow = boardCol = 7;

    fillBoard(0, 2, 0, 2, CellValue::wall);
    fillBoard(0, 2, 5, 7, CellValue::wall);
    fillBoard(5, 7, 0, 2, CellValue::wall);
    fillBoard(5, 7, 5, 7, CellValue::wall);
    board[3][3].setValue(CellValue::empty);
}   

void PegSolitaire::setBoardDiamond () {
    initBoard(9, 9, CellValue::peg);
    boardRow = boardCol = 9;

    for (int i = 0, n = 4; i < 4; ++i, --n)
        for (int j = 0; j < n; ++j)
            board[i][j].setValue(CellValue::wall);

    for (int i = 0, n = 5; i < 4; ++i, ++n)
        for (int j = n; j < 9; ++j)
            board[i][j].setValue(CellValue::wall);

    for (int i = 5, n = 1; i < 9; ++i, ++n)
        for (int j = 0; j < n; ++j)
            board[i][j].setValue(CellValue::wall);

    for (int i = 5, n = 8; i < 9; ++i, --n)
        for (int j = n; j < 9; ++j)
            board[i][j].setValue(CellValue::wall);
    board[4][4].setValue(CellValue::empty);
}   

void PegSolitaire::setBoardTriangular () {
    const int N = 5;    // 5x5 triangular board (for display)
    boardRow = N;
    boardCol = N + N - 1;

    initBoard(boardRow, boardCol, CellValue::wall);
    board[0][boardRow - 1].setValue(CellValue::empty);

    for (int i = 1; i < boardRow; ++i) {
        CellValue cv = CellValue::peg;
        int end = boardCol - boardRow + i;
        for (int j = boardRow - 1 - i; j <= end; ++j) {
            board[i][j].setValue(cv);
            cv = cv == CellValue::peg ? CellValue::junction : CellValue::peg;  
        }
    }
}  

void PegSolitaire::autoResize () {
    int row = board.size();
    int col = 0;

    for (int i = 0; i < board.size(); ++i)
        if (board[i].size() > col)
            col = board[i].size();
    
    int boardSize = bigger(row, col);

    // Resize board rows
    if (row < boardSize) board.resize(boardSize);

    // Resize board col
    for (int i = 0; i < boardSize; ++i) 
        for (int j = board[i].size(); j < boardSize; ++j)
            board[i].push_back(Cell(i, j, CellValue::wall));
}

void PegSolitaire::initBoard (int row, int col, PegSolitaire::CellValue initValue) {
    board.resize(row);
    for (int i = 0; i < row; ++i) {
        board[i].resize(col);
        for (int j = 0; j < col; ++j)
            board[i][j].setCell(i, j, initValue);
    }
}

void PegSolitaire::fillBoard (int outStart, int outEnd, int inStart, int inEnd, CellValue v) {
    for (int i = outStart; i < outEnd; ++i)
        for (int j = inStart; j < inEnd; ++j)
            board[i][j].setValue(v);
}

/***********************************************************************************
 *                                     Cell
 **********************************************************************************/

PegSolitaire::Cell::Cell (int cellRow, int cellCol, CellValue cellValue) {
    setCol(cellCol);
    setRow(cellRow); 
    setValue(cellValue); 
}

PegSolitaire::Cell::Cell () : row(0), col(0), value(CellValue::undefined) {/* Empty */}

void PegSolitaire::Cell::getCell (int & cellRow, int & cellCol, CellValue & cellValue) const {
    cellCol = col; 
    cellRow = row; 
    cellValue = value; 
}

inline PegSolitaire::CellValue PegSolitaire::Cell::getValue () const {
    return value;
}

inline int PegSolitaire::Cell::getCol () const {
    return col;
}

inline int PegSolitaire::Cell::getRow () const {
    return row;
}

int PegSolitaire::Cell::setCell (int cellRow, int cellCol, CellValue cellValue) {
    int r = setRow(cellRow);
    if (r == RETURN_SUCCESS) {
        r = setCol(cellCol);
        if (r == RETURN_SUCCESS) 
            r = setValue(cellValue);
    }
    return r; 
}

int PegSolitaire::Cell::setRow (int cellRow) {
    int r;
    //! boardSize
    if (cellRow >= 0) {
        row = cellRow;
        r = RETURN_SUCCESS;
    } 
    else {
        cerr << "(!) Invalid row value: " << cellRow << endl;
        r = RETURN_FAILURE;
    }
    return r;
}

int PegSolitaire::Cell::setCol (int cellCol) {
    int r;
    //! boardSize
    if (cellCol >= 0) {
        col = cellCol;
        r = RETURN_SUCCESS;
    }  
    else {
        cerr << "(!) Invalid coloumn value: " << cellCol << endl;
        r = RETURN_FAILURE;
    }
    return r;
}

int PegSolitaire::Cell::setValue (CellValue cellValue) {
    int r;
    if (cellValue == CellValue::undefined || cellValue == CellValue::empty || cellValue == CellValue::wall ||
        cellValue == CellValue::junction || cellValue == CellValue::peg) {
        value = cellValue;
        r = RETURN_SUCCESS;
    }
    else {
        cerr << "(!) Invalid cell value: " << static_cast<char>(cellValue) << endl;
        r = RETURN_FAILURE;
    }
    return r;
}

/***********************************************************************************
 *                                     Movement
 **********************************************************************************/

PegSolitaire::Movement::Movement (int startRow, int startCol, Direction dir)
    : row(startRow), col(startCol), direction(dir) { /* Empty  */ }

PegSolitaire::Movement::Movement ()
    : row(0), col(0), direction(Direction::undefined) { /* Empty  */ }

int PegSolitaire::Movement::setMovement (const string & mov) {
    int r = RETURN_FAILURE;
    string str_mov = strUp(mov);;
    string str_col;
    int i;
    
    // Check if entered string is in a proper movement format e.g A3-D, GB3-U
    for (i = 0; i < str_mov.size() && isLetter(str_mov[i]); ++i)
        str_col.push_back(str_mov[i]);

    if (i > 0) {
        int j;
        string str_row;
        for (j = i; j < str_mov.size() && isDigit(str_mov[j]); ++j)
            str_row.push_back(str_mov[j]);
        
        if (j != i && str_mov[j] == '-') {
            ++j;
            if (str_mov[j] == 'U' || str_mov[j] == 'D' || str_mov[j] == 'R' || str_mov[j] == 'L') {
                if (j + 1 == str_mov.size()) {
                    // Convert str values to proper type and assign them
                    int row = strToInt(str_row);
                    int col = colToInt(str_col);
                    Direction dir = static_cast<Direction>(str_mov[j]);
                    setStartRow(row);
                    setStartCol(col);
                    setDirection(dir);
                    r = RETURN_SUCCESS;
                }
            } 
        }
    }
    return r;
}

int PegSolitaire::Movement::setStartRow (int startRow) {
    int r; 
    if (startRow >= 0) { //!
        row = startRow;
        r = RETURN_SUCCESS;
    } 
    else {
        cout << "(!) Invalid row value\n";
        r = RETURN_FAILURE;
    }
    return r;
}

int PegSolitaire::Movement::setStartCol (int startCol) {
    int r; 
    if (startCol >= 0) { //!
        col = startCol;
        r = RETURN_SUCCESS;
    } 
    else {
        cout << "(!) Invalid row value\n";
        r = RETURN_FAILURE;
    }
    return r;
}

int PegSolitaire::Movement::setDirection (Direction dir) {
    int r;
    if (direction == Direction::down  || 
        direction == Direction::up    ||
        direction == Direction::right ||
        direction == Direction::left  ||
        direction == Direction::undefined) {
        direction = dir;
        r = RETURN_SUCCESS;
        }
    else {
        cerr << "(!) Invalid direction type\n";
        r = RETURN_FAILURE;
    }
    return r;
}

inline int PegSolitaire::Movement::getStartRow () const {
    return row;
}

inline int PegSolitaire::Movement::getStartCol () const {
    return col;
}

inline PegSolitaire::Direction PegSolitaire::Movement::getDirection () const {
    return direction;
} 

ostream & operator<< (ostream & outStream, const PegSolitaire::Movement & mov) {
    outStream << mov.colToStr() << mov.getStartRow() << '-' << static_cast<char>(mov.getDirection());
    return outStream;
}

int PegSolitaire::Movement::colToInt (const string & str_col) const {
    const int ENG_LETTER_NUM = 26;    // The number of letter in english alphabet
    int int_col = 0;
    
    for (int i = 0; i < str_col.size(); ++i)
        int_col = int_col * ENG_LETTER_NUM + str_col[i] - 'A';
    return int_col;
}

string PegSolitaire::Movement::colToStr () const {
    const int ENG_LETTER_NUM = 26;    // The number of letter in english alphabet
    int int_col = getStartCol();
    string str_col;

    do {
        str_col.push_back('A' + int_col % ENG_LETTER_NUM);
        int_col /= ENG_LETTER_NUM;
    } while (int_col > 0);
    
    // Reverse the string r
    for (int s = 0, e = str_col.size() - 1; s < e; ++s, --e) {
        int tmp = str_col[s];
        str_col[s] = str_col[e];
        str_col[e] = tmp;
    }
    
    return str_col;
}


/***********************************************************************************
 *                                    Helper
 *********************************************************************************/

string setw (int n) {
    string w;
    for (int i = 0; i < n; ++i)
        w.push_back(' ');
    return w;
}

bool getChoice (string prompt) {
    bool exit = false;
    string s;
    char c;

    cout << prompt;
    do {
        cin >> s;
        c = toupper(s[0]);
        
        if (c == 'Y' || c == 'N')
            exit = true;
        else
            cout << "Please select a proper option: ";
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
            cout << "Please select a proper option: ";
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
                 << "Please select a proper option: ";
    } while (exit == false);
    cin.get();  // consume \n
    return r;
}

void enterTo (const string & prompt) {
    cout << prompt; cin.get();
}

inline int bigger (int n1, int n2) {return n1 > n2 ? n1 : n2;}

inline bool isInRange (int n, int lb, int ub) {return lb <= n && n <= ub;}
 
inline bool isDigit (char c) {return '0' <= c && c <= '9';}

bool isLetter (char c) {
    c = charUp(c);
    return 'A' <= c && c <= 'Z';
}

int strToInt (const string & s) {
    int r = 0;

    // RETURN_FAILURE is negative constant value
    for (int i = 0; i < s.length() && r != RETURN_FAILURE; ++i)
        if (isdigit(s[i]))
            r = r * 10 + s[i] - '0';
        else 
            r = RETURN_FAILURE;

    return r;
}

char charUp (char c) {
    return 'a' <= c && c <= 'z' ? 'A' + c - 'a' : c;
}

string strUp (const string & s) {
    string sUp;
    int i = 0;
    while (i < s.size())
        sUp.push_back(charUp(s[i++]));
    sUp[i] = '\0'; 
    return sUp;
}

PegSolitaire::Command whichCommand(const string & str) {
    string command = strUp(str);

    if (command == "LOAD")
        return PegSolitaire::Command::load;
    else if (command == "SAVE")
        return PegSolitaire::Command::save;
    else if (command == "EXIT")
        return PegSolitaire::Command::exit;
    else
        return PegSolitaire::Command::undefined;
} 

void listGames (const vector<PegSolitaire> & game) {
    if (game.size() > 0) {
        cout << "ACTIVE GAME SELECTION MENU\n";
        for (int i = 0; i < game.size(); ++i)
            cout << i + 1 << ") Game" << i + 1 << endl;
    }
    else {
        cerr << "There is no active game exist yet\n";
        enterTo("Enter to continue ");
    }
}

void displayGameRules () {
    cout << "================ GAME MODES ================\n\n"
         << "// There are 2 type of games: Human mode and Computer mode\n"
         << "Human Mode\n"
         << "------------------\n"
         << "// All the movements done by the player\n"
         << "Computer Mode\n"
         << "------------------\n"
         << "// All the movements done by the computer\n\n";

    enterTo("Enter to next page "); cout << endl;
    
    cout << "================ CELL NOTATION ================\n\n"
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

    enterTo("Enter to next page "); cout << endl;

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

    enterTo("Enter to next page "); cout << endl;

    cout << "================= COMMANDS ================\n\n"
         << "/****/ You can use these commands when you see terminal sign(>>)\n\n"
         << "// exit: Exits from current place\n"
         << "       usage: exit\n\n"
         << "// save: Saves the current progress of the game\n"
         << "       usage: save filename.txt\n\n"
         << "// load: Loads the spefic game\n"
         << "       usage: load filename.txt\n\n";

    enterTo("Enter to go back MAIN MENU "); 
}

void displayAllBoardTypes () {
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
