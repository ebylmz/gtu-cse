GCCPARAMS = -Wextra -Wall
CC = gcc
OUT = pCp

# pCp <buffer size> <#of consumers> <source dirname> <dest dirname>
BUFF_SIZE = 100
NUM_CONS = 10
SRC1 = ../data/srcdir
SRC2 = ../data/srcdir2
DST = dstdir

pCp: pCp.c util.c
	$(CC) $(GCCPARAMS) $^ -o $@ 

run1: $(OUT) 
	./$(OUT) $(BUFF_SIZE) $(NUM_CONS) $(SRC1) $(DST)

run2: $(OUT) 
	./$(OUT) $(BUFF_SIZE) $(NUM_CONS) $(SRC1) $(SRC2) $(DST)

# small buffer, small thread pool 
test1: $(OUT)	
	./$(OUT) 1 1 $(SRC1) $(DST)

# small buffer, large thread pool 
test2: $(OUT)
	./$(OUT) 10 500 $(SRC1) $(DST)

# large buffer, small thread pool 
test3: $(OUT)
	./$(OUT) 500 10 $(SRC1) $(DST)

# large buffer, large thread pool 
test4: $(OUT)
	./$(OUT) 5000 1000 $(SRC1) $(DST)

# large buffer, moderate thread pool 
test5: $(OUT)
	./$(OUT) 2000 50 $(SRC1) $(DST)

valgrind: $(OUT) 
	valgrind --leak-check=full --track-origins=yes --show-leak-kinds=all --gen-suppressions=all \
	-s ./$(OUT) $(BUFF_SIZE) $(NUM_CONS) $(SRC1) $(DST)

clean:
	rm -rf $(OUT) $(DST)