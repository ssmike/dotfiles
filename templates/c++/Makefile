main: clean main.o source
	g++ ./main.o -o main -pg -w
clean:
	rm -f ./main
	rm -rf ./*.o
	rm -f ./gmon.out
main.o:
	g++ -ggdb -c ./main.cpp -pg -w -std=c++11

check-syntax:
	clang++ -o nul -Wall -S ${CHK_SOURCES}
source:
	./standardfilter.sh
edit:
	vim ./main.cpp
run:
	./main
	cp ./outp /tmp/outp

