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
LIBDIR = $(realpath $(DIR)/../lib)
CC = gcc
CFLAGS = -m32 -Wall
RUNTIME = runtime.o
RUNTIME_C = latte-runtime.c

.PHONY: all
all: $(LATC) $(PY_TREE) $(PY_LATTE) $(RUNTIME)

$(LATC):
	@echo -e "#!/bin/sh\nPYTHONPATH=\"$(LIBDIR)\" python2 \"$(DIR)/$(PY_MAIN)\" " \
		"-r \"$(LIBDIR)/runtime.o\" "'"$$@"' > $@
	@chmod +x $@

$(PY_LATTE): $(G_LATTE)
	$(ANTLR) $<

$(PY_TREE): $(G_TREE) $(PY_LATTE)
	$(ANTLR) $<

$(RUNTIME): $(RUNTIME_C)
	$(CC) $(CFLAGS) -c $< -o $@

.PHONY: clean
clean:
	rm -f $(PY_LATTE) $(PY_TREE) *.{pyc,pyo,tokens} $(LATC) $(RUNTIME)
