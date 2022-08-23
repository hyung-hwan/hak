OPTS := -gnata -gnatW8 -gnatwa -gnatya -gnatyb -gnatyk -gnatyn -gnatyp -gnat2005 ##-gnatyt
all: hello hello2 hello3

hello: hello.adb
	gnat make $(OPTS) hello

hello2: hello2.adb
	gnat make $(OPTS) hello2

hello3: hello3.adb
	gnat make $(OPTS) hello3

clean:
	gnat clean hello
	gnat clean hello2
	gnat clean hello3

