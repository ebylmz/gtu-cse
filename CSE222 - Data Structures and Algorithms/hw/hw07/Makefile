JC = javac
R = java
JFlags = 
SUPER = src/
SUB1 = bst/
SUB2 = heap/
TEST = test/
DRIVER = Driver

muo:
	$(JC) $(JFlags) $(SUPER)$(SUB1)*.java $(SUPER)$(SUB2)*.java $(SUPER)$(TEST)*.java -d bin
	$(R) -cp bin $(SUPER)$(TEST)$(DRIVER)

javadoc:
	javadoc $(SUPER)$(SUB1)*.java $(SUPER)$(SUB2)*.java -d javadoc

clean:
	rm javadoc/ bin/ -r