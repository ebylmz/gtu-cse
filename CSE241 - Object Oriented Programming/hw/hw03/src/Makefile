all: main

main: main.o pegSolitaire.o
	g++ --std=c++11 main.o pegSolitaire.o -o main

main.o: main.cpp
	g++ --std=c++11 main.cpp -c

pegSolitaire.o: pegSolitaire.cpp
	g++ --std=c++11 pegSolitaire.cpp -c

clean:
	rm -f main *.o