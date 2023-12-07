JC = javac
R = java
Packet = setDoY
JFlags = 

muo:
	$(JC) $(JFlags) $(Packet)/DayOfYearSet.java $(Packet)/TestDayOfYearSet.java
	$(R) $(Packet).TestDayOfYearSet

javadoc:
	javadoc -d doc/ $(Packet)/*.java

rebuild: clean muo

clean:
	rm -rf $(Packet)/*.class doc/ 