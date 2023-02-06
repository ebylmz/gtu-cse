JFLAGS = -g
JC = javac
RM = *Pack/
JAVADOC = doc

.SUFFIXES: .java .class

.java.class:
	$(JC) $(JFLAGS) $*.java

CLASSES = *Pack/*.java

default: classes

classes: $(CLASSES:.java=.class)

clean:
	rm -rf $(RM)*.class $(JAVADOC)/*

javadoc:
	javadoc -d doc *Pack/*.java

# run:
# 	java main