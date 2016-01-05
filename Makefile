LATC = latc_x86

.PHONY: all
all: $(LATC)

$(LATC):
	$(MAKE) $(MAKEOPTS) -C src
	mv src/$@ .

.PHONY: clean
clean:
	rm -f $(LATC)
	$(MAKE) -C src clean

