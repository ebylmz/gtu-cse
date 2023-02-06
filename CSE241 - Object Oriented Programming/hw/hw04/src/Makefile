all: test

test: test.o dayOfYearSet.o 
	g++ --std=c++11 test.o dayOfYearSet.o -o test

test.o: test.cpp
	g++ --std=c++11 test.cpp -c

dayOfYearSet.o: dayOfYearSet.cpp
	g++ --std=c++11 dayOfYearSet.cpp -c

valgrind: test
	valgrind ./test

clean:
	rm -rf *.o test