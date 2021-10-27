PREFIX?=/usr/local

install:
	mkdir -p ${PREFIX}/lib/untask
	mkdir -p ${PREFIX}/bin
	cp -r src/ main.rkt ${PREFIX}/lib/untask
	sed "s|/usr/local|${PREFIX}|" <run.bash >${PREFIX}/bin/untask
	chmod +x ${PREFIX}/bin/untask
	echo ${PREFIX}

.PHONY: clean
clean:
	rm -f main untask
