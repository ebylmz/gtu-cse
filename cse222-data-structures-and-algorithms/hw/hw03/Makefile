JC = javac
R = java
SUPER = src/cityplanner/
SUB1 = src/util/
SUB2 = src/street/
SUB3 = src/structure/
TEST = test/
DRIVER = Test
JFlags = 


muo:
	$(JC) $(JFlags) $(SUPER)*.java $(SUB1)*.java $(SUB2)*.java $(SUB3)*.java $(TEST)*.java  -d classfiles
	$(R) -cp classfiles $(TEST)$(DRIVER)

javadoc:
	javadoc $(SUPER)*.java $(SUB1)*.java $(SUB2)*.java $(SUB3)*.java $(TEST)*.java  -d javadoc

clean:
	rm javadoc/ classfiles/ -r