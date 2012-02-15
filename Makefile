all: target alloyIG.jar
	ghc -XDeriveDataTypeable -isrc src/Main.hs -outputdir target --make -o claferIG

target:
	@if test -n "$(shell cabal info haxml | grep 'Latest version installed' | grep 'Not installed')"; then \
		echo "[ERROR] Missing haxml library. Try 'cabal install haxml'."; false; \
	fi
	@if test -n "$(shell cabal info cmdargs | grep 'Latest version installed' | grep 'Not installed')"; then \
		echo "[ERROR] Missing cmdargs library. Try 'cabal install cmdargs'."; false; \
	fi
	@if test -n "$(shell cabal info executable-path | grep 'Latest version installed' | grep 'Not installed')"; then \
		echo "[ERROR] Missing executable-path library. Try 'cabal install executable-path'."; false; \
	fi
	@if test ! -f "alloy4.jar"; then \
		echo "[ERROR] Missing alloy4.jar. Try copying the jar into the current directory."; false; \
	fi
	mkdir -p target

alloyIG.jar: src/manifest src/org/clafer/ig/AlloyInterface.java src/edu/mit/csail/sdg/alloy4compiler/parser/AlloyCompiler.java
	javac -cp "alloy4.jar" -d target src/org/clafer/ig/AlloyInterface.java src/edu/mit/csail/sdg/alloy4compiler/parser/AlloyCompiler.java
	jar cfm alloyIG.jar src/manifest -C target org/clafer/ig/ -C target edu
	
clean:
	rm -rf target
	rm -f alloyIG.jar
	rm -f claferIG
