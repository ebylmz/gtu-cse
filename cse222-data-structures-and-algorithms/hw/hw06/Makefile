JC = javac
R = java
SUPER = src/
SUB1 = hashing/
SUB2 = sorting/
SUB3 = bst/
TEST = test/
DRIVER = Driver
JFlags = 

muo:
	$(JC) $(JFlags) $(SUPER)$(SUB1)*.java $(SUPER)$(SUB2)*.java $(SUPER)$(SUB3)*.java $(SUPER)$(TEST)*.java -d classfiles
	$(R) -cp classfiles $(SUPER)$(TEST)$(DRIVER)

javadoc:
	javadoc $(SUPER)$(SUB1)*.java $(SUPER)$(SUB2)*.java $(SUPER)$(SUB3)*.java -d javadoc

clean:
	rm javadoc/ classfiles/ -r