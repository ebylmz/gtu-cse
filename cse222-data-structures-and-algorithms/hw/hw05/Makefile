JC = javac
R = java
SUPER = src/
DRIVER = Test
JFlags = 

muo:
	$(JC) $(JFlags) $(SUPER)*.java -d classfiles
	$(R) -cp classfiles $(SUPER)$(DRIVER)

javadoc:
	javadoc $(SUPER)*.java  -d javadoc

clean:
	rm javadoc/ classfiles/ -r