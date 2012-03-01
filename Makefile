UNAME := $(shell uname | tr "A-Z" "a-z")
ONAME := $(shell uname -o | tr "A-Z" "a-z")
MNAME := $(shell uname -m | tr "A-Z" "a-z")

ifeq ($(UNAME), linux)
	ifeq ($(MNAME), i686)
        LIB := x86-linux/libminisatprover*
    endif
	ifeq ($(MNAME), x86_64)
        LIB := x86-linux/libminisatprover*
    endif
endif
ifeq ($(UNAME), windows)
	ifeq ($(MNAME), i686)
        LIB := x86-windows/minisatprover*
    endif
	ifeq ($(MNAME), x86_64)
        LIB := x86-windows/minisatprover*
    endif
endif
ifeq ($(ONAME), cygwin)
	ifeq ($(MNAME), i686)
        LIB := x86-windows/minisatprover*
    endif
	ifeq ($(MNAME), x86_64)
        LIB := x86-windows/minisatprover*
    endif
endif
ifeq ($(UNAME), darwin)
	ifeq ($(MNAME), i686)
        LIB := x86-mac/libminisatprover*
    endif
	ifeq ($(MNAME), x86_64)
        LIB := x86-mac/libminisatprover*
    endif
endif


all: alloyIG.jar lib install

install:
	cabal install --bindir=.

# this takes the version from the .cabal file. Need to run install first to produce Paths_claferIG.hs 
newVersion:
	ghc -isrc src/dateVer.hs dist/build/autogen/Paths_claferIG.hs -outputdir dist/build --make -o dateVer
	./dateVer > src/Version.hs

lib:
	@if test -z $(LIB); then \
		echo "[WARNING] Did not find a minisat prover binary suitable for your system. You may need to build the binary yourself."; \
	else \
		unzip alloy4.jar $(LIB) -d lib; \
		chmod +x lib/$(LIB); \
		cp lib/$(LIB) lib; \
	fi
	
# Build takes less time. For ease of development.
build: alloyIG.jar
	ghc -XDeriveDataTypeable -isrc src/Main.hs -outputdir dist/build --make -o claferIG

alloyIG.jar: src/manifest src/org/clafer/ig/AlloyIG.java src/manifest src/org/clafer/ig/AlloyIGException.java src/edu/mit/csail/sdg/alloy4compiler/parser/AlloyCompiler.java
	@if test ! -f "alloy4.jar"; then \
		echo "[ERROR] Missing alloy4.jar. Try copying the jar into the current directory."; false; \
	fi
	mkdir -p dist/javabuild
	javac -cp "alloy4.jar" -d dist/javabuild src/org/clafer/ig/AlloyIG.java src/org/clafer/ig/AlloyIGException.java src/edu/mit/csail/sdg/alloy4compiler/parser/AlloyCompiler.java
	jar cfm alloyIG.jar src/manifest -C dist/javabuild org/clafer/ig/ -C dist/javabuild edu
	
clean:
	rm -rf dist
	rm -f alloyIG.jar
	rm -f claferIG
	rm -rf lib
