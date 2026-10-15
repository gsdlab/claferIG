ifeq ($(OS),Windows_NT)
EXE := .exe
endif

ALLOY_JAR := alloy6.2.0.jar
ALLOY_URL := https://github.com/AlloyTools/org.alloytools.alloy/releases/download/v6.2.0/org.alloytools.alloy.dist.jar

# The MiniSat prover built with optimization (see native/minisatprover/CMakeLists.txt).
MINISATPROVER_REPO := https://github.com/AlloyTools/minisatprover
MINISATPROVER_COMMIT := 54bf0f5d07024a7b91052c8371de75accc70c4a9
ifeq ($(shell uname -s),Darwin)
	MINISATPROVER := lib/libminisatprover.dylib
else
	MINISATPROVER := lib/libminisatprover.so
endif

# Calling `make` should only build
all: alloyIG.jar $(MINISATPROVER) build

# Calling `make install to=<target directory>` should only install
install:
	mkdir -p $(to)
	cp -f $(ALLOY_JAR) $(to)
	mkdir -p $(to)/lib
	cp -f $(MINISATPROVER) $(to)/lib
	cp -f alloyIG.jar $(to)
	cp -f LICENSE $(to)/
	cp -f CHANGES.md $(to)/claferIG-CHANGES.md
	cp -f README.md $(to)/claferIG-README.md
	cp `stack path --local-install-root`/bin/claferIG$(EXE) $(to)

# Build takes less time. For ease of development.
build: alloyIG.jar
	stack build
	cp alloyIG.jar `stack path --local-install-root`/bin/
	cp $(ALLOY_JAR) `stack path --local-install-root`/bin/
	if test -f $(MINISATPROVER); then cp -R lib `stack path --local-install-root`/bin/; fi

alloyIG.jar: $(ALLOY_JAR) src/manifest src/org/clafer/ig/AlloyIG.java src/org/clafer/ig/Util.java src/org/clafer/ig/AlloyIGException.java src/org/alloytools/solvers/natv/minisatprover/FixedMiniSatProver.java
	mkdir -p dist/javabuild
	javac --release 17 -cp "$(ALLOY_JAR)" -d dist/javabuild src/org/clafer/ig/AlloyIG.java src/org/clafer/ig/Util.java src/org/clafer/ig/AlloyIGException.java src/org/alloytools/solvers/natv/minisatprover/FixedMiniSatProver.java
	jar cfm alloyIG.jar src/manifest -C dist/javabuild org/clafer/ig/ -C dist/javabuild org/alloytools/

$(MINISATPROVER): native/minisatprover/CMakeLists.txt native/minisatprover/MiniSatProver.cpp native/minisatprover/AlloyMiniSatProver.h
	rm -rf native/minisatprover/repo dist/minisatprover
	git clone -q $(MINISATPROVER_REPO) native/minisatprover/repo
	git -C native/minisatprover/repo checkout -q $(MINISATPROVER_COMMIT)
	cmake -S native/minisatprover -B dist/minisatprover
	cmake --build dist/minisatprover
	mkdir -p lib
	cp dist/minisatprover/$(notdir $(MINISATPROVER)) $@

.PHONY : test

test:
	stack test --no-run-tests  
	cp alloyIG.jar `stack path --dist-dir`/build/model-tests/
	cp $(ALLOY_JAR) `stack path --dist-dir`/build/model-tests/
	if test -f $(MINISATPROVER); then cp -R lib `stack path --dist-dir`/build/model-tests/; fi
	cp alloyIG.jar `stack path --dist-dir`/build/claferIG/
	cp $(ALLOY_JAR) `stack path --dist-dir`/build/claferIG/
	if test -f $(MINISATPROVER); then cp -R lib `stack path --dist-dir`/build/claferIG/; fi
	stack test

clean:
	stack clean
	rm -f alloyIG.jar
	rm -rf lib native/minisatprover/repo dist/minisatprover
	rm -rf tools

tags:
	hasktags --ctags --extendedctag .

codex:
	codex update
	mv codex.tags tags

$(ALLOY_JAR):
	@echo "[WARNING] Missing $(ALLOY_JAR). Downloading..."
	curl -fsSL -o $(ALLOY_JAR) $(ALLOY_URL)
