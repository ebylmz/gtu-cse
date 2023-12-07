#include <iostream>
#include <string>
#include <vector>
#include <chrono>
#include <thread>
#include "boardGame2D.h"

using std::string;
using std::vector;
using std::cout;
using std::cin;
using std::endl;
using namespace std::this_thread;   // sleep_for, sleep_until
using namespace std::chrono;        // nanoseconds, system_clock, seconds

namespace GameEBY {
    void randDirection (vector<Direction> & v) {
        v.resize(4);
        for (Direction & d : v)
            d = Direction::undef;

        for (int i = 0, n; i < 4; ++i) {
            // define an unfilled place
            do n = rand() % 4;
            while (v[n] != Direction::undef);

            // assign the direction to the ramdomly selected place
            switch (i) {
                case 0: v[n] = Direction::up; break;
                case 1: v[n] = Direction::down; break;
                case 2: v[n] = Direction::right; break;
                case 3: v[n] = Direction::left; break;
            }     
        }
    }

    BoardGame2D::BoardGame2D ()
    : _numOfMov(0), _boardScore(0) {/* body is empty */}

    void BoardGame2D::playUser () {
        vector<string> possibleMove;
        string mov;
        while (!endGame()) {
            print(cout);
            if (possibleMove.size() > 0) {
                cout << "Possible Moves:\n";
                for (string m : possibleMove)
                    cout << m << " ";
                cout << endl;
                possibleMove.resize(0); 
            }
            
            cout << "Movement: ";
            cin >> mov;
            
            if (mov == "EXIT")
                break;
            else if (mov == "SHOW")
                possibleMove = nextMoves();
            else {
                int rvalue = playUser(mov);
                if (rvalue != SUCCESS && rvalue != GAME_IS_OVER) {
                    // display the rvalue for a short period of time
                    printRvalue(rvalue);
                    cin.get();  //! I dont' why this needed but needed
                    sleep_for(nanoseconds(10));
                    sleep_until(system_clock::now() + seconds(1));
                }
            }
        } 
        print(cout);
        cout << "GAME IS OVER\n";
    }        

    void BoardGame2D::playAutoAll () {
        char c;
        string command;
        while (!endGame()) {
            print(cout);
            /*
            */
            cout << "\nEnter to continue ";
            // check if any command entered
            for (cin.get(c); c != '\n'; cin.get(c))
                command.push_back(c);

            // check if user enter any command
            if (!command.empty()) {
                if (command == "EXIT")
                    break;
                else
                    command.clear();
            }
                
            playAuto();
        }
        print(cout);
        cout << "GAME IS OVER\n";    
    }

    void BoardGame2D::playVector (vector<BoardGame2D *> & gameV) {
        for (int i = 0; i < gameV.size(); ++i)
            gameV[i]->playAutoAll();
    }

    void BoardGame2D::printRvalue (int rvalue) {
        switch (rvalue) {
            case BAD_INPUT:
                cout << "Invalid input format ";
                break;
            case INVALID_MOV:
                cout << "Invalid movement ";
                break;
            case GAME_IS_OVER:
                cout << "Game is over ";
                break;
            case SUCCESS:
                cout << "Successfull execution ";
                break;
        }    
    }

    int BoardGame2D::boardScore () const {return _boardScore;}
    
    int BoardGame2D::numOfMov () const {return _numOfMov;}

    ostream & operator<< (ostream & outs, const BoardGame2D & game) {
        game.print(outs);
        return outs;
    }

    void BoardGame2D::setBoardScore (int score) {
        _boardScore = score;
    }
    
    void BoardGame2D::setNumOfMov (int numOfMov) {
        _numOfMov = numOfMov;
    }
}