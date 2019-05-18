build:
	chicken-install -n #not installing, only building

install:
	chicken-install #you must be be root!

clean:
	rm -f *.sh *.so *.o *.link *.import.scm
