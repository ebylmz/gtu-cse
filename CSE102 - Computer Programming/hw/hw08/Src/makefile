target:	hw8

hw8_lib:	hw8_lib.h hw8_lib.c
			gcc -ansi -pedantic-errors -c hw8_lib.c

hw8_main:	hw8_lib.h hw8_main.c
			gcc -ansi -pedantic-errors -c hw8_main.c


hw8:	hw8_main hw8_lib
		gcc hw8_lib.o hw8_main.c -o hw8

clean:
		rm hw8