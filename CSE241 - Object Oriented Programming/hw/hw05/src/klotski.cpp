#include <iostream>
#include <string>
#include <vector>
#include "klotski.h"
#include "ansiEscape.h"

using std::cout;
using std::cerr;
using std::endl;
using std::string;
using std::vector;
using AnsiEscape::clearScreen;
using AnsiEscape::setCursor;
using AnsiEscape::setBackgroundColor;

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
}

namespace GameEBY {
    Klotski::Block::Block (int startRow, int startCol, int endRow, int endCol, Color color, string name)
    : _sRow(startRow), _sCol(startCol), _eRow(endRow), _eCol(endCol), _color(color), _name(name) {}

    int Klotski::Block::startRow () const {return _sRow;}
    int Klotski::Block::startCol () const {return _sCol;}
    int Klotski::Block::endRow () const {return _eRow;}
    int Klotski::Block::endCol () const {return _eCol;}
    Color Klotski::Block::color () const {return _color;}
    string Klotski::Block::name () const {return _name;}

    void Klotski::Block::setStartPosition (int row, int col) {
        _sRow = row;
        _sCol = col;
    }

    void Klotski::Block::setEndPosition (int row, int col) {
        _eRow = row;
        _eCol = col;
    }

    void Klotski::Block::setColor (Color color) {
        _color = color;
    }
    
    void Klotski::Block::setName (string name) {
        _name = name;
    }

    bool Klotski::Block::overlap (const Block & other) const {
        // define the upper and lower blocks
        const Block * upper, * lower;
        if (startRow() <= other.startRow()) {
            upper = this;
            lower = &other;
        }
        else {
            upper = &other;
            lower = this;
        }
        
        if (lower->startRow() < upper->endRow()) {
            // define the right and left blocks
            const Block * left, * right;
            if (startCol() <= other.startCol()) {
                left = this;
                right = &other;
            }
            else {
                left = &other;
                right = this;
            } 

            if (right->startCol() < left->endCol()) 
                return true;
        }
        return false;
    } 

    bool Klotski::Block::inside (const Block & outer) const {
        // if this block is inside of the outer block then followings must be true 
        // start point should be larger than or equal to start point of outer block
        // end point should be smaller than or equal to end point of outer block 
        return  outer.startRow() <= startRow() && outer.startCol() <= startCol() &&
                endRow() <= outer.endRow() && endCol() <= outer.endCol();
    }

    void Klotski::Block::print () const {
        setBackgroundColor(color());
        
        for (int i = startRow(); i < endRow(); ++i) {
            setCursor(i, startCol());
            for (int j = startCol(); j < endCol(); ++j)
                cout << " ";
        }

        setCursor(startRow(), startCol());
        cout << name();
    
        setBackgroundColor(); // turn back the default background
    }

    bool Klotski::Block::operator== (const Block & other) const {
        return  startRow() == other.startRow() &&
                startCol() == other.startCol() &&
                endRow() == other.endRow() &&
                endCol() == other.endCol() &&
                color() == other.color() &&
                name() == other.name();
    }

    Klotski::Klotski ()
    : BoardGame2D(), _border(2, 3, 12, 19, Color::blue, "border"), _lastMoved(nullptr) {}

    void Klotski::initialize () {
        _blocks.clear();

        // initialize the border
        _border.setColor(Color::blue);
        _border.setStartPosition(2, 3);
        _border.setEndPosition(12, 19);
        _border.setName("border");

        // add 4 4x2 block
        _blocks.push_back(Block(2,  3, 6,   7, Color::cyan, "R1"));
        _blocks.push_back(Block(2, 15, 6,  19, Color::cyan, "R2"));
        _blocks.push_back(Block(6,  3, 10,  7, Color::cyan, "R3"));
        _blocks.push_back(Block(6, 15, 10, 19, Color::cyan, "R4"));

        // add 1 2x4 block
        _blocks.push_back(Block(6, 7, 8, 15, Color::yellow, "R5"));

        // add the target block whose dimensions are 1 4x4 block
        _blocks.push_back(Block(2, 7, 6, 15, Color::red, "S1"));

        // add 4 2x2 block
        _blocks.push_back(Block(8,   7, 10, 11, Color::green, "S2"));
        _blocks.push_back(Block(8,  11, 10, 15, Color::green, "S3"));
        _blocks.push_back(Block(10,  3, 12, 7, Color::green, "S4"));
        _blocks.push_back(Block(10, 15, 12, 19, Color::green, "S5"));
    }

    int Klotski::playUser (const string & mov) {
        Direction dir;
        int index;
        if (!strToMov(mov, index, dir))
            return BAD_INPUT;
        else if (!move(index, dir))
            return INVALID_MOV;
        else
            return SUCCESS;
    }
    
    bool Klotski::playAuto () {
        //! SOLUTION 1 
        /*
        if (!endGame()) {
            // get all the possible movements and randomly choose one of them
            vector<string> posssibleMoves = nextMoves();
            srand(time(NULL));
            int i = rand() % posssibleMoves.size();
            playUser(posssibleMoves[i]);
        }
        return false;
        */

        //! SOLUTION 2 
        if (!endGame()) {
            vector<Direction> v;
            
            // selects a random block and try to move it by trying random directions 
            srand(time(NULL));
            int index = rand() % _blocks.size();

            for (int i = 0; i < _blocks.size(); ++i) {
                // try each randomly selected direction for selected block
                randDirection(v);  
                for (Direction & d : v)
                    if (_lastMoved != &_blocks[index] && move(index, d))
                        return true;
                index = (index + 1 == _blocks.size()) ? 0 : index + 1;
            }

            // this function only return false if the game is ended
            index = block(_lastMoved->name());
            return   index != -1 &&
                    (move(index, Direction::up) ||
                     move(index, Direction::down) ||
                     move(index, Direction::right) ||
                     move(index, Direction::left) );
        }
        return false;
    }

    vector<string> Klotski::nextMoves () {
        vector<string> possibleMoves;

        for (Block & b : _blocks) {
            if (validMovement(b, Direction::up))
                possibleMoves.push_back(b.name() + "-U");
            
            if (validMovement(b, Direction::down))
                possibleMoves.push_back(b.name() + "-D");
            
            if (validMovement(b, Direction::right))
                possibleMoves.push_back(b.name() + "-R");
            
            if (validMovement(b, Direction::left))
                possibleMoves.push_back(b.name() + "-L");
        }

        return possibleMoves;
    }

    bool Klotski::endGame () const {
        // if target block is the end point then game is over
        // end points are (8, 7), (12, 15) at the front of the door

        int i = block("S1");    // S1 is the target block
        return  _blocks[i].startRow() == 8 && _blocks[i].startCol() == 7 &&  
                _blocks[i].endRow() == 12 && _blocks[i].endCol() == 12; 
    }

    void Klotski::print (ostream & outs) const {
        cout << *this << endl;
        cout << "move: " << numOfMov() << endl;
    }

    ostream & operator<< (ostream & outs, const Klotski & game) {
        clearScreen();
        for (auto b : game._blocks)
            b.print();

        game.printBoardBorder();
        outs << endl;
        return outs;
    }   

    void Klotski::printBoardBorder () const {
        // 5x4 puzzle (5 row, 4 col)
        const int h = 5;
        const int w = 4;
        // set border height and width
        int verticalDis = h * 2 + 2; 
        int horizantalDis = w * 4 + 2;
        
        setBackgroundColor(_border.color());        

        // top horizontal border
        setCursor();
        for (int i = 0; i < horizantalDis; ++i) 
            cout << ' ';
        
        // bottom horizontal border
        setCursor(h * 2 + 2);  
        for (int i = 0; i < 6; ++i) 
            cout << ' ';

        setCursor(h * 2 + 2, 1 + 6 + 8);  
        for (int i = 0; i < 6; ++i) 
            cout << ' ';

        // left vertical border
        setCursor();
        for (int i = 0; i < verticalDis; ++i) 
            cout << "  " << endl;

        // right vertical border
        for (int i = 0; i < verticalDis; ++i) {
            setCursor(1 + i, 1 + 2 + 4 * w);
            cout << "  ";
        } 
        setBackgroundColor(); // turn back the default background color
    }

    bool Klotski::validMovement (const Block & target, Direction dir) {
        // set the position of targetBlock after movement done
        int sRow = target.startRow();
        int sCol = target.startCol();
        int eRow = target.endRow();
        int eCol = target.endCol();
        switch (dir) {
            case Direction::up:
                sRow -= 2;
                eRow -= 2;
                break;
            case Direction::down:
                sRow += 2;
                eRow += 2;
                break;
            case Direction::left:
                sCol -= 4;
                eCol -= 4;
                break;
            case Direction::right:
                sCol += 4;
                eCol += 4;
                break;
        }

        // target will be located to the place movedBlock placed
        Block movedBlock(sRow, sCol, eRow, eCol);
        // check if movedBlock is inside the border and 
        // does not overlap with other block except itself (target)
        if (movedBlock.inside(_border)) {
            for (Block & b : _blocks)
                if (b.name() != target.name() && b.overlap(movedBlock))     
                    return false;
            return true;
        }
        return false;  
    }


    bool Klotski::move (int index, Direction dir) {
        Block & targetBlock      = _blocks[index];

        // set the position of targetBlock after movement done
        int sRow = targetBlock.startRow();
        int sCol = targetBlock.startCol();
        int eRow = targetBlock.endRow();
        int eCol = targetBlock.endCol();
        switch (dir) {
            case Direction::up:
                sRow -= 2;
                eRow -= 2;
                break;
            case Direction::down:
                sRow += 2;
                eRow += 2;
                break;
            case Direction::left:
                sCol -= 4;
                eCol -= 4;
                break;
            case Direction::right:
                sCol += 4;
                eCol += 4;
                break;
        }

        // targetBlock will be located to the place movedBlock placed
        Block movedBlock(sRow, sCol, eRow, eCol);
        // check if movedBlock is inside the border and 
        // does not overlap with other block except itself (targetBlock)
        if (movedBlock.inside(_border)) {
            for (Block & b : _blocks)
                if (b.name() != targetBlock.name())    
                    if (b.overlap(movedBlock)) 
                        return false;

            // make movement by updating targetBlock positions
            targetBlock.setStartPosition(sRow, sCol); 
            targetBlock.setEndPosition(eRow, eCol);
            _lastMoved = &targetBlock; 
            setNumOfMov(numOfMov() + 1);
            return true;
        }
        return false;
    }

    int Klotski::block (const string & name) const {
        for (int i = 0; i < _blocks.size(); ++i)
            if (_blocks[i].name() == name)
                return i;
        return -1;
    }

    bool Klotski::strToMov (const string & str, int & index, Direction & dir) const {
        string mov(strUp(str));

        // example movement: S4-R
        if (mov.size() == 4 && isLetter(mov[0]) && isDigit(mov[1]) && mov[2] == '-' &&
        (mov[3] == 'U' || mov[3] == 'D' || mov[3] == 'R' || mov[3] == 'L')) {
            // extract cell name and check if it's exist such a block
            index = block(mov.substr(0, 2));
            if (index != -1) {
                // extract direction
                dir = static_cast<GameEBY::Direction>(mov[3]);
                return true;
            }
        }
        return false;
    }
}