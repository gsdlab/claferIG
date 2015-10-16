ifeq ($(OS),Windows_NT)
EXE := .exe
endif

ifeq ($(OS),Windows_NT)
	LIB := x86-windows/minisatprover*
else
	UNAME := $(shell uname -s)
	ifeq ($(UNAME), Linux)
		MNAME := $(shell uname -m | tr "A-Z" "a-z")
		ifeq ($(MNAME), i686)
			LIB := x86-linux/libminisatprover*
		endif
		ifeq ($(MNAME), x86_64)
			# amd64 is a nickname for x86_64
			LIB := amd64-linux/libminisatprover*
		endif
	endif
	ifeq ($(UNAME),Darwin)
		WGET_COMMAND := curl -O
		LIB := x86-mac/libminisatprover*
	endif
endif

# Calling `make` should only build
all: alloyIG.jar lib build

# Calling `make install to=<target directory>` should only install
install:
	mkdir -p $(to)
	mkdir -p $(to)/lib
	cp -f lib/*minisatprover* $(to)/lib
	cp -f alloy4.2.jar $(to)
	cp -f alloyIG.jar $(to)
	cp -f LICENSE $(to)/
	cp -f CHANGES.md $(to)/claferIG-CHANGES.md
	cp -f README.md $(to)/claferIG-README.md
	cp `stack path --local-install-root`/bin/claferIG$(EXE) $(to)

# Build takes less time. For ease of development.
build: alloyIG.jar
	stack build

alloyIG.jar: alloy4.2.jar src/manifest src/org/clafer/ig/AlloyIG.java src/manifest src/org/clafer/ig/Util.java src/org/clafer/ig/AlloyIGException.java src/edu/mit/csail/sdg/alloy4compiler/parser/AlloyCompiler.java
	mkdir -p dist/javabuild
	javac  -cp "alloy4.2.jar" -d dist/javabuild src/org/clafer/ig/AlloyIG.java src/org/clafer/ig/Util.java src/org/clafer/ig/AlloyIGException.java src/edu/mit/csail/sdg/alloy4compiler/parser/AlloyCompiler.java
	jar cfm alloyIG.jar src/manifest -C dist/javabuild org/clafer/ig/ -C dist/javabuild edu

.PHONY : test lib

lib: alloy4.2.jar
	@if test -z $(LIB); then \
		echo "[WARNING] Did not find a minisat prover binary suitable for your system. You may need to build the binary yourself."; \
	else \
		unzip alloy4.2.jar $(LIB) -d lib; \
		chmod +x lib/$(LIB); \
		cp lib/$(LIB) lib; \
	fi

test:
	cp alloyIG.jar `stack path --dist-dir`/build/test-suite/
	cp alloy4.2.jar `stack path --dist-dir`/build/test-suite/
	cp -R lib  `stack path --dist-dir`/build/test-suite/
	cp alloyIG.jar `stack path --dist-dir`/build/claferIG/
	cp alloy4.2.jar `stack path --dist-dir`/build/claferIG/
	cp -R lib  `stack path --dist-dir`/build/claferIG/
	stack test

clean:
	stack clean
	rm -f alloyIG.jar
	rm -rf tools

tags:
	hasktags --ctags --extendedctag .

codex:
	codex update
	mv codex.tags tags

WGET_COMMAND := wget
ifeq ($(OS),Windows_NT)
	ifeq ($(shell which wget), which: wget: unkown command)
		pacman -S make wget
	endif
else
	UNAME_S := $(shell uname -s)
	ifeq ($(UNAME_S),Darwin)
		WGET_COMMAND := curl -O
	endif
endif

alloy4.2.jar:
	@if test ! -f "alloy4.2.jar"; then \
		echo "[WARNING] Missing alloy4.2.jar. Downloading...";  \
		$(WGET_COMMAND) http://alloy.mit.edu/alloy/downloads/alloy4.2_2015-02-22.jar; \
		mv alloy4.2_2015-02-22.jar alloy4.2.jar; \
	fi
