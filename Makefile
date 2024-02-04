all: vm

vm: vm.c
	$(CC) -o $@ $< -Wall -pedantic -std=c11 -g
