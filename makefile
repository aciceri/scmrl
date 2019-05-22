build:
	rm -f scmrl
	chicken-install -n #not installing, only building

install:
	chicken-install #you must be be root!

run: scmrl
	./scmrl || reset #if it doesn't correctly exit it resets the terminal

clean:
	rm -f *.sh *.so *.o *.link *.import.scm
