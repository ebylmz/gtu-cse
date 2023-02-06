TARGET = gpp_interpreter

GPP_C_OBJ = gpp.c

YACC_OBJ = parser.y

YACC_C_OBJ = parser.tab.c

LEX_OBJ = lexer.l

LEX_C_OBJ = lexer.c

all: 
	yacc -o $(YACC_C_OBJ) -d $(YACC_OBJ)
	flex -o $(LEX_C_OBJ) $(LEX_OBJ)
	gcc $(GPP_C_OBJ) $(LEX_C_OBJ) $(YACC_C_OBJ) -o $(TARGET)

clean:
	rm $(TARGET) $(YACC_C_OBJ) $(LEX_C_OBJ) gpp.o parser.tab.h