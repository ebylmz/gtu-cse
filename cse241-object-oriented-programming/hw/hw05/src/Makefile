CC = g++
CFLAGS = --std=c++11

all: main

main: main.o boardGame2D.o pegSolitaire.o eightPuzzle.o klotski.o ansiEscape.o 
	$(CC) main.o boardGame2D.o pegSolitaire.o eightPuzzle.o klotski.o ansiEscape.o -o main

main.o: main.cpp
	$(CC) $(CFLAGS) main.cpp -c

boardGame2D.o: boardGame2D.cpp
	$(CC) $(CFLAGS) boardGame2D.cpp -c

pegSolitaire.o: pegSolitaire.cpp
	$(CC) $(CFLAGS) pegSolitaire.cpp -c

eightPuzzle.o: eightPuzzle.cpp
	$(CC) $(CFLAGS) eightPuzzle.cpp -c

klotski.o: klotski.cpp
	$(CC) $(CFLAGS) klotski.cpp -c

ansiEscape.o: ansiEscape.cpp 
	$(CC) $(CFLAGS) ansiEscape.cpp -c

rebuild: clean all

clean :
	rm -rf *.o main