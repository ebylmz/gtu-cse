CC = gcc
GCCPARAMS = -Wextra -Wall
OUT = makeFileSystem

BLOCK_SIZE = 4	# 4KB
DISK_FILE = mySystem.dat

target: $(OUT)

$(OUT): $(OUT).c fs.c
	$(CC) $(GCCPARAMS) $^ -o $@

# makeFileSystem [block size] [disk file name>]\n", argv[0]);
run: $(OUT) 
	./$(OUT) $(BLOCK_SIZE) $(DISK_FILE)

valgrind: $(OUT) 
	valgrind --leak-check=full --track-origins=yes --show-leak-kinds=all --gen-suppressions=all \
	./$(OUT) $(BLOCK_SIZE) $(DISK_FILE)

clean:
	rm -rf $(OUT) $(DISK_FILE)