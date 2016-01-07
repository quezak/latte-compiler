LATC = latc_x86
RUNTIME = runtime.o

.PHONY: all
all: 
	$(MAKE) $(MAKEOPTS) -C src
	ln -fs src/$(LATC) .
	cd lib && ln -fs ../src/$(RUNTIME) .

.PHONY: clean
clean:
	rm -f $(LATC) lib/$(RUNTIME) lib/antlr{,3}/*.py{c,o} 
	$(MAKE) -C src clean

