OPTS := -gnata -gnatW8 -gnatwa -gnatya -gnatyb -gnatyk -gnatyn -gnatyp ##-gnatyt
all: hello hello2 hello3

hello: hello.adb
	gnat make $(OPTS) hello && valgrind ./hello

hello2: hello2.adb
	gnat make $(OPTS) hello2 && valgrind ./hello2

hello3: hello3.adb
	gnat make $(OPTS) hello3 && valgrind ./hello3

clean:
	rm -rf *.o *.ali hello hello2 hello3
