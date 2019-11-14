untask: main wrapper.bash
	rm -f untask
	cat wrapper.bash main > untask
	chmod +x untask

main: main.rkt info.rkt src
	raco exe $<

.PHONY: clean
clean:
	rm -f main untask
