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

clean:
	rm -rf $(DIST)
