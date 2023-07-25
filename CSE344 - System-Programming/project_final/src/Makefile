CC = gcc
GCCPARAMS = -Wextra -Wall
OUT_SERVER = bibakBoxServer
OUT_CLIENT = bibakBoxClient
LOG_FILE = clilog.txt

# create the client directory in the client current directory
CLIENT_DIRNAME = clidir
SERVER_DIRNAME = svdir
PORT_NUMBER = 8080
THREAD_POOL_SIZE = 10

target: $(OUT_SERVER) $(OUT_CLIENT) 

$(OUT_SERVER): $(OUT_SERVER).c util.c sync.c
	$(CC) $(GCCPARAMS) $^ -o $@

$(OUT_CLIENT): $(OUT_CLIENT).c util.c sync.c
	$(CC) $(GCCPARAMS) $^ -o $@

# bibakBoxServer [directory] [threadPoolSize] [portnumber]
runServer: $(OUT_SERVER) 
	./$(OUT_SERVER) $(SERVER_DIRNAME) $(THREAD_POOL_SIZE) $(PORT_NUMBER)

# bibakBoxClient [dirName] [portnumber]
runClient: $(OUT_CLIENT) 
	./$(OUT_CLIENT) $(CLIENT_DIRNAME) $(PORT_NUMBER)

valgrindServer: $(OUT_SERVER) $(OUT_CLIENT) 
	valgrind --leak-check=full --track-origins=yes --show-leak-kinds=all --gen-suppressions=all \
	./$(OUT_SERVER) $(SERVER_DIRNAME) $(THREAD_POOL_SIZE) $(PORT_NUMBER)

valgrindClient: $(OUT_SERVER) $(OUT_CLIENT) 
	valgrind --leak-check=full --track-origins=yes --show-leak-kinds=all --gen-suppressions=all -s \
	./$(OUT_CLIENT) $(CLIENT_DIRNAME) $(PORT_NUMBER)

cleanDir:
	rm -rf $(CLIENT_DIRNAME)/* \
	rm -rf $(SERVER_DIRNAME)/*

cpFile:
	cp a.c $(CLIENT_DIRNAME)/

clean:
	rm -rf $(OUT_SERVER) $(OUT_CLIENT) $(LOG_FILE)