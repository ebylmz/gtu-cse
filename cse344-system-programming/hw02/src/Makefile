GCCPARAMS = -Wextra -Wall
CC = gcc
OUT = shell

target: shell

shell: shell.c
	$(CC) $(GCCPARAMS) $< -o $@ 

memory:
	valgrind --leak-check=full --track-origins=yes --show-leak-kinds=all --gen-suppressions=all -s ./$(OUT)

clean:
	rm $(OUT) *.log