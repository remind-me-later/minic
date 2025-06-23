all: test

test.o: test.asm
	nasm -f elf64 test.asm -o test.o

print_char.o: print_char.asm
	nasm -f elf64 print_char.asm -o print_char.o

test: print_char.o test.o
	mold print_char.o test.o -o test

clean:
	rm -f *.o test