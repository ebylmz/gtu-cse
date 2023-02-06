#include <iostream>
#include <vector>
#include "boardGame2D.h"
#include "pegSolitaire.h"
#include "eightPuzzle.h"
#include "klotski.h"

using std::vector;
using std::cout;
using std::endl;
using GameEBY::BoardGame2D;
using GameEBY::PegSolitaire;
using GameEBY::EightPuzzle;
using GameEBY::Klotski;

void test (BoardGame2D * g, int gmode);
// plays the given game in two different game mode 
// gmode = 0 for auto mode 
// gmode = 1 for player mode 
void test ();
// tests all the games in auto mode

int main (void) {
    // change the gamemode
    // gmode 0 auto mode 
    // gmode 1 user mode 
    int gmode = 0;
    
    BoardGame2D * g = new Klotski;
    /*
    BoardGame2D * g = new EightPuzzle;
    BoardGame2D * g = new PegSolitaire;
    */
    test(g, gmode);
    
    return 0;
}


void test (BoardGame2D * g, int gmode) {
    g->initialize();
    if (gmode == 0) 
        g->playAutoAll();
    else if (gmode == 1) 
        g->playUser();
}

void test () {
    BoardGame2D * g[6];
    g[0] = new PegSolitaire;
    g[1] = new PegSolitaire;
    g[2] = new EightPuzzle;
    g[3] = new EightPuzzle;
    g[4] = new Klotski;
    g[5] = new Klotski;
    
    for (auto p : g)
        p->playAutoAll();
}