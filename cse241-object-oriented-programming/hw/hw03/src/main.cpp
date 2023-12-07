#include "pegSolitaire.h"

int PegSolitaire::pegNumber = 0;
int PegSolitaire::gameNumber = 0;

int main (void) {
    vector<PegSolitaire> game;  // Keeps all the games that currently playing
    string filename;
    
    bool exit = false;
    int choice1, choice2;

    cout << "=================================================\n"
         << "-------->>>>>>>>> PEG SOLITAIRE <<<<<<<<<--------\n"
         << "=================================================\n\n";

        //  << "     (# Welcome to Peg Solitaire Universe #)\n\n";
    
    do {
        cout << "\nMAIN MENU\n"
             << "1) Show Game Rules\n"
             << "2) Start New Game\n"
             << "3) Load Game\n"
             << "4) Continue Active Game\n"
             << "5) Compare Game\n"
             << "6) Peg Numbers at All The Active Games\n"
             << "0) Exit\n";
        choice1 = getChoice("Choose an option: ", 0, 5);
        cout << endl;
        switch (choice1) {
            case 0:
                exit = getChoice("Are you sure you want to exit the game? (y or n) ");
                break;
            case 1:
                displayGameRules(); //! add page number enter to next page...
                break;
            case 2: 
                game.push_back(PegSolitaire());
                break;
            case 3:
                cout << "Enter the file name: ";
                cin  >> filename;
                game.push_back(PegSolitaire(filename));
                // If something bad happen and game cannot started properly neglect that game
                if (game[game.size() - 1].getGameActivity() == false) game.pop_back();                    
                break;
            case 4:
                //! KEEPING GAME NAMES IN PEGSOLITARE CLASS
                //! DISPLAY THE GAME NAME
                if (game.size() > 0) {
                    cout << "ACTIVE GAME SELECTION MENU\n";
                    listGames(game);
                    cout << "0) Go back to MAIN MENU\n";
                    choice1 = getChoice("Choose an option: ", 0, game.size());
                    if (choice1 != 0)
                        game[choice1 - 1].startGame();
                }
                else {
                    cerr << "There is no active game exist yet\n";
                    enterTo("Enter to continue ");
                }
                break;
            case 5:
                listGames(game);
                cout << "0) Go back to MAIN MENU\n";
                choice1 = getChoice("Choose 1st game: ", 0, game.size());
                choice2 = getChoice("Choose 2nd game: ", 0, game.size());
                if (choice1 != 0 && choice2 != 0) {
                    --choice1, --choice2;   // come back 0 index notation
                    if (game[choice1].compare(game[choice2]))
                        cout << "1st game has more peg than 2nd game\n";
                    else
                        cout << "2nd game has more peg than 1st game\n";
                    cout << "Peg numbers:"
                         << "1st game: " << game[choice1].getPegNumber()
                         << "2nd game: " << game[choice2].getPegNumber();
                    enterTo("Enter to continue ");
                }
                break;
            case 6:
                cout << "Active Games: " << PegSolitaire::getActiveGameNumber() << endl
                     << "Peg Number  : " << PegSolitaire::getPegNumber() << endl;
                enterTo("Enter to continue ");
                break;
        }
    } while (! exit);   
    cout << "EXIT\n";
    return 0;
}