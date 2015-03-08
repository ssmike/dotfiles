CFLAGS+=-g  -Wall -Wextra -pedantic -std=c++11 -O2 -Wshadow -Wformat=2 -Wfloat-equal -Wconversion -Wlogical-op -Wcast-qual -Wcast-align 
LDFLAGS+=-pg -g
CC=g++
LD=g++

all: main source

main: main.o
	$(LD) ./main.o -o main $(LDFLAGS)
clean:
	rm -f ./main
	rm -rf ./*.o
	rm -f ./gmon.out
main.o: main.cpp
	sed -i 's/\(^[^#].*\$\)/\# \1/g' ./gdb-start
	$(CC) -c ./main.cpp -pg $(CFLAGS)

check-syntax:
	clang++ -o nul -Wall -S ${CHK_SOURCES}
source:
	./standardfilter.sh
edit:
	vim -p ./main.cpp ./gdb-start
run: main
	./main < ./inp > ./outp
	cp ./outp /tmp/outp
	head -n 30 ./outp
debug: main
	gdb ./main -x ./gdb-start
