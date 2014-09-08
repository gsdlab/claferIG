TOOL_DIR = tools

# Calling `make` should only build
all: alloyIG.jar lib build

# Calling `make install to=<target directory>` should only install
install:
	mkdir -p $(to)
	mkdir -p $(to)/lib
	mkdir -p $(to)/tools
	cp -f lib/*minisatprover* $(to)/lib
	cp -f tools/alloy4.jar $(to)/tools
	cp -f alloyIG.jar $(to)
	cp -f LICENSE $(to)/
	cp -f CHANGES.md $(to)/claferIG-CHANGES.md
	cp -f README.md $(to)/claferIG-README.md
	cp -f -R IDEs $(to)
	cabal install --bindir=$(to) --ghc-option="-O"

# Removes current build and makes a clean new one (Don't use if starting from scratch!)
cleanEnv:
	make clean
	ghc-pkg unregister claferIG
	rm `which claferIG`
	make 

# this takes the version from the .cabal file. Need to run install first to produce Paths_claferIG.hs 
newVersion:
	ghc -isrc src/dateVer.hs dist/build/autogen/Paths_claferIG.hs -outputdir dist/build --make -o dateVer
	./dateVer > src/Language/Clafer/IG/Version.hs

init:
	cabal sandbox init --sandbox=../.clafertools-cabal-sandbox
	cabal install --only-dependencies

# Build takes less time. For ease of development.
build: alloyIG.jar
	cabal configure
	cabal build

alloyIG.jar: src/manifest src/org/clafer/ig/AlloyIG.java src/manifest src/org/clafer/ig/Util.java src/org/clafer/ig/AlloyIGException.java src/edu/mit/csail/sdg/alloy4compiler/parser/AlloyCompiler.java
	$(MAKE) -C $(TOOL_DIR)
	mkdir -p dist/javabuild
	javac  -source 1.6 -target 1.6 -cp "tools/alloy4.jar" -d dist/javabuild src/org/clafer/ig/AlloyIG.java src/org/clafer/ig/Util.java src/org/clafer/ig/AlloyIGException.java src/edu/mit/csail/sdg/alloy4compiler/parser/AlloyCompiler.java
	jar cfm alloyIG.jar src/manifest -C dist/javabuild org/clafer/ig/ -C dist/javabuild edu

lib:
	$(MAKE) -C $(TOOL_DIR) lib
	
test:
	# Only test a subset of the suite. The other cases do not work yet.
	cabal configure --enable-tests
	cabal build
	mkdir dist/build/test-suite/lib
	cp alloyIG.jar dist/build/test-suite/lib
	cabal test
	./claferIG --all=4 -s dist/run test/suite/backquoted.cfr
	./claferIG --all=4 -s dist/run test/suite/BobsTeam.cfr
	./claferIG --all=4 -s dist/run test/suite/inconsistent.cfr
	./claferIG --all=4 -s dist/run test/suite/PersonFingers.cfr
	./claferIG --all=4 -s dist/run test/suite/waitingLine.cfr
	./claferIG --all=4 -s dist/run test/suite/subclaferCardinality.cfr
	
clean:
	rm -rf dist
	rm -f alloyIG.jar
	rm -f claferIG
	rm -rf tools/x86-linux
	rm -rf tools/amd64-linux
	rm -rf tools/x86-windows
	rm -rf tools/x86-mac