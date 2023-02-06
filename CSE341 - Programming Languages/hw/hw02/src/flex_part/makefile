FLEX = flex
COMPILER = gcc

FILE = gpp_lexer

flex: ${FILE}.l
	${FLEX} -o $(FILE).c ${FILE}.l
	${COMPILER} ${FILE}.c -o ${FILE}

clean:
	rm ${FILE} ${FILE}.c