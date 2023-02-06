CC = g++
CFLAGS = -std=c++11

all: test-Iterable test-GTUSet test-GTUVector test-GTUArray 

# Test Iterable
test-Iterable: test-Iterable.o 
	$(CC) $(CFLAGS) test-Iterable.o -o test-Iterable 

test-Iterable.o: src/test-Iterable.cpp
	$(CC) $(CFLAGS) src/test-Iterable.cpp -c

# TEST GTUSet
test-GTUSet: test-GTUSet.o 
	$(CC) $(CFLAGS) test-GTUSet.o -o test-GTUSet 

test-GTUSet.o: src/test-GTUSet.cpp
	$(CC) $(CFLAGS) src/test-GTUSet.cpp -c

# TEST GTUArray
test-GTUArray: test-GTUArray.o 
	$(CC) $(CFLAGS) test-GTUArray.o -o test-GTUArray

test-GTUArray.o: src/test-GTUArray.cpp
	$(CC) $(CFLAGS) src/test-GTUArray.cpp -c

# TEST GTUVector
test-GTUVector: test-GTUVector.o  
	$(CC) $(CFLAGS) test-GTUVector.o -o test-GTUVector

test-GTUVector.o: src/test-GTUVector.cpp
	$(CC) $(CFLAGS) src/test-GTUVector.cpp -c

rebuild: clean all

clean: 
	rm -rf *.o test*