GCCPARAMS = -Wextra -Wall
OUT = appendMeMore dup verify_dup

target: appendMeMore dup verify_dup

appendMeMore: appendMeMore.c
	gcc $(GCCPARAMS) $< -o $@ 

dup: dup.c
	gcc $(GCCPARAMS) $< -o $@ 

verify_dup: verify_dup.c
	gcc $(GCCPARAMS) $< -o $@ 

clean:
	rm $(OUT) test1.txt test2.txt