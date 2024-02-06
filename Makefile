all: vm

FLAGS=-Wall -pedantic -std=c11 -O3
#FLAGS=-Wall -pedantic -std=c11 -g -DDEBUG

vm: vm.c
	$(CC) -o $@ $< $(FLAGS)
