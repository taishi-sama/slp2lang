ld -o $2 -dynamic-linker /lib/ld-linux-x86-64.so.2 /usr/lib/crt1.o /usr/lib/crti.o -lc $1  /usr/lib/crtn.o
