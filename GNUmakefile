SOURCES=*.hs Tests/*.hs
EXE=LoadingPlanes
DIST=dist

all: compile lint

compile: $(SOURCES)
	cabal install --haddock-executables --disable-documentation

run: compile
	$(EXE)

lint: $(SOURCES)
	hlint $(SOURCES)

doc: $(SOURCES)
	cabal haddock --executables

clean:
	rm -rf $(DIST)
