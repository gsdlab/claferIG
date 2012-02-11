all: target alloyIG.jar
	ghc -XDeriveDataTypeable -isrc src/Main.hs -outputdir target --make -o claferIG

target:
	mkdir -p target

alloyIG.jar: src/manifest src/org/clafer/ig/AlloyInterface.java src/edu/mit/csail/sdg/alloy4compiler/parser/AlloyCompiler.java
	javac -cp "alloy4.jar" -d target src/org/clafer/ig/AlloyInterface.java src/edu/mit/csail/sdg/alloy4compiler/parser/AlloyCompiler.java
	jar cfm alloyIG.jar src/manifest -C target org/clafer/ig/ -C target edu
	
clean:
	rm -rf target
	rm -f alloyIG.jar
	rm -f claferIG
