all: install

install: dist/javabuild alloyIG.jar
	cabal install --bindir=.
	
# Build takes less time. For ease of development.
build: dist/javabuild alloyIG.jar
	ghc -XDeriveDataTypeable -isrc src/Main.hs -outputdir dist/build --make -o claferIG

dist/javabuild:
	@if test ! -f "alloy4.jar"; then \
		echo "[ERROR] Missing alloy4.jar. Try copying the jar into the current directory."; false; \
	fi
	mkdir -p dist/javabuild

alloyIG.jar: src/manifest src/org/clafer/ig/AlloyInterface.java src/edu/mit/csail/sdg/alloy4compiler/parser/AlloyCompiler.java
	javac -cp "alloy4.jar" -d dist/javabuild src/org/clafer/ig/AlloyInterface.java src/edu/mit/csail/sdg/alloy4compiler/parser/AlloyCompiler.java
	jar cfm alloyIG.jar src/manifest -C dist/javabuild org/clafer/ig/ -C dist/javabuild edu
	
clean:
	rm -rf dist
	rm -f alloyIG.jar
	rm -f claferIG
