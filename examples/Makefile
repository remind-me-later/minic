.POSIX:
.PHONY: all run clean

all: test.exe

%.o: %.s
	as --64 -o $@ $<

test.exe: print_char.o test.o
	mold print_char.o test.o -o test.exe

run: test.exe
	./test.exe

clean:
	rm -f *.o test.exe