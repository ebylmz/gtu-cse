all: timedemo

main: dtime.o timedemo.o
	g++ --std=c++11 timedemo.o dtime.p -o timedemo

timedemo.o:
	g++ --std=c++11 timedemo.cpp -c

dtime.o:
	g++ --std=c++11 dtime.cpp -c

clean:
	rm -rf *.o timedemo