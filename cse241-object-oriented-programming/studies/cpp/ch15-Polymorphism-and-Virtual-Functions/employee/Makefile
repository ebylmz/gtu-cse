all: main

CC = g++	
CFLAGS = --std=c++11 

main: main.o employee.o hourlyEmployee.o salariedEmployee.o commissionEmployee.o basePlusCommissionEmployee.o 
	$(CC) main.o employee.o hourlyEmployee.o salariedEmployee.o commissionEmployee.o basePlusCommissionEmployee.o -o main

main.o:
	$(CC) $(CFLAGS) main.cpp -c

employee.o: 
	$(CC) $(CFLAGS) employee.cpp -c

hourlyEmployee.o: 
	$(CC) $(CFLAGS) hourlyEmployee.cpp -c

salariedEmployee.o: 
	$(CC) $(CFLAGS) salariedEmployee.cpp -c

commissionEmployee.o: 
	$(CC) $(CFLAGS) commissionEmployee.cpp -c

basePlusCommissionEmployee.o: 
	$(CC) $(CFLAGS) basePlusCommissionEmployee.cpp -c

clean:
	rm -rf *.o main