GCCPARAMS = -Wextra -Wall
CC = gcc
OUT = biboServer biboClient

target: $(OUT)

biboServer: biboServer.c common.c sync.c
	$(CC) $(GCCPARAMS) $^ -o $@ 

biboClient: biboClient.c common.c
	$(CC) $(GCCPARAMS) $^ -o $@ 

memory:
	valgrind --leak-check=full --track-origins=yes --show-leak-kinds=all --gen-suppressions=all -s ./biboServer ../data/here 3 3

clean:
	rm $(OUT) *.log