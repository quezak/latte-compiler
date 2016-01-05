LATC = latc_x86
G_LATTE = Latte.g
G_TREE = LatteTreeBuilder.g
PY_LATTE = LatteLexer.py  LatteParser.py
PY_TREE = LatteTreeBuilder.py
PY_MAIN = LatteMain.py
JAVA = java
JFLAGS = -classpath ../lib/antlr3.jar:../lib/antlr.jar:../lib/stringtemplate.jar -Djava.library.path=/lib:/usr/lib:../lib -Dfile.encoding=utf-8
ANTLR = $(JAVA) $(JFLAGS) org.antlr.Tool
DIR = $(shell pwd)

.PHONY: all
all: $(LATC) $(PY_TREE) $(PY_LATTE)

$(LATC):
	@echo -e "#!/bin/sh\nPYTHONPATH=$(DIR)/../lib python2 $(DIR)/$(PY_MAIN) "'"$$@"' > $@
	@chmod +x $@

$(PY_LATTE): $(G_LATTE)
	$(ANTLR) $<

$(PY_TREE): $(G_TREE) $(PY_LATTE)
	$(ANTLR) $<

# TODO compile all .pys manually?

.PHONY: clean
clean:
	rm -f $(PY_LATTE) $(PY_TREE) *.{pyc,pyo,tokens} $(LATC) ../lib/antlr3/*.py{c,o}
