all: driver

driver: hourlyEmployee.o salariedEmployee.o employee.o driver.o
	g++ --std=c++11 hourlyEmployee.o salariedEmployee.o employee.o driver.o -o driver

driver.o: driver.cpp
	g++ --std=c++11 driver.cpp -c

hourlyEmployee.o: hourlyEmployee.cpp employee.o
	g++ --std=c++11 hourlyEmployee.cpp -c

salariedEmployee.o: salariedEmployee.cpp employee.o
	g++ --std=c++11 salariedEmployee.cpp -c

employee.o: employee.cpp
	g++ --std=c++11 employee.cpp -c

clean:
	rm -rf *.o driver

# for windows
# g++ --std=c++11 employee.cpp -c
# g++ --std=c++11 salariedEmployee.cpp -c
# g++ --std=c++11 hourlyEmployee.cpp -c
# g++ --std=c++11 driver.cpp -c
# g++ --std=c++11 hourlyEmployee.o salariedEmployee.o employee.o driver.o -o driver