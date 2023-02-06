all: test

test: test.o peg_solitaire.o
	g++ test.o peg_solitaire.o -o test

test.o: test.cpp
	g++ --std=c++11 test.cpp -c

peg_solitaire.o: peg_solitaire.cpp
	g++ --std=c++11 peg_solitaire.cpp -c

clean:
	rm -f *.o test