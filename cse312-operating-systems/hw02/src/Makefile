GCCPARAMS = -Wextra -Wall # -DDEBUG
CC = g++
OUT = operateArrays

# operateArrays 12 5 10 LRU inverted diskFileName.dat

ARGV_1 = 12  # frame size (pow2) 
ARGV_2 = 5 	 # pyhsical frame number (pow2)
ARGV_3 = 10  # virtual frame number (pow2)
ARGV_4_LRU = LRU 
ARGV_4_SC = SC 
ARGV_4_WSClock = WSClock 
ARGV_5 = inverted
ARGV_6 = diskFile.dat

RESULT_FILE = result.txt

target: $(OUT)

operateArrays: operateArrays.cpp memory.cpp simulation.cpp
	$(CC) $(GCCPARAMS) $^ -o $@ 

runLRU: $(OUT)
	./$(OUT) $(ARGV_1) $(ARGV_2) $(ARGV_3) $(ARGV_4_LRU) $(ARGV_5) $(ARGV_6)

runSC: $(OUT)
	./$(OUT) $(ARGV_1) $(ARGV_2) $(ARGV_3) $(ARGV_4_SC) $(ARGV_5) $(ARGV_6)

runWSClock: $(OUT)
	./$(OUT) $(ARGV_1) $(ARGV_2) $(ARGV_3) $(ARGV_4_WSClock) $(ARGV_5) $(ARGV_6) 

runX: $(OUT)
	./$(OUT) 14 $(ARGV_2) 8 $(ARGV_4_LRU) $(ARGV_5) $(ARGV_6) 

runSmallFrame: $(OUT)
	./$(OUT) 9 $(ARGV_2) 13 $(ARGV_4_LRU) $(ARGV_5) $(ARGV_6) 

runMediumFrame: $(OUT)
	./$(OUT) 12 $(ARGV_2) 10 $(ARGV_4_LRU) $(ARGV_5) $(ARGV_6) 

runLargeFrame: $(OUT)
	./$(OUT) 15 $(ARGV_2) 7 $(ARGV_4_LRU) $(ARGV_5) $(ARGV_6) 

valgrind: $(OUT)
	valgrind --leak-check=full --track-origins=yes --show-leak-kinds=all --gen-suppressions=all \
	-s ./$(OUT) $(ARGV_1) $(ARGV_2) $(ARGV_3) $(ARGV_4) $(ARGV_5) $(ARGV_6)

clean:
	rm -f $(OUT) $(ARGV_6) $(RESULT_FILE)