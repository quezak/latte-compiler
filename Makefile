LATC = latc_x86

.PHONY: all
all: 
	$(MAKE) $(MAKEOPTS) -C src
	cp src/$(LATC) .

.PHONY: clean
clean:
	rm -f $(LATC)
	$(MAKE) -C src clean

