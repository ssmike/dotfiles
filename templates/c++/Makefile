CFLAGS+=-g  -Wall -Wextra -pedantic -std=c++11 -Wshadow -Wformat=2 -Wfloat-equal -Wconversion -Wlogical-op -Wcast-qual -Wcast-align
LDFLAGS+=-pg -g
CC=g++
LD=g++
VIM=vim

TASKNAME=main

all: $(TASKNAME) source

$(TASKNAME): $(TASKNAME).o
	$(LD) $(TASKNAME).o -o $(TASKNAME) $(LDFLAGS)
clean:
	rm -f $(TASKNAME)
	rm -f *.o
	rm -f gmon.out
$(TASKNAME).o: $(TASKNAME).cpp
	sed -i 's/\(^[^#].*\$\)/\# \1/g' gdb-start
	$(CC) -c $(TASKNAME).cpp -pg $(CFLAGS)

source:
	./standardfilter.sh
	xsel -ib < /tmp/source
edit:
	$(VIM) $(TASKNAME).cpp
run: $(TASKNAME)
	./$(TASKNAME)
	cp outp /tmp/outp
	cat outp
debug: $(TASKNAME)
	gdb $(TASKNAME) -x gdb-start
