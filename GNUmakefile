SOURCES=Main.hs
EXE=bin/LoadingPlanes
DOCS=doc/

all: compile doc lint

compile: $(SOURCES)
	ghc --make -Wall $^ -outputdir bin -o $(EXE)

run: all
	$(EXE)

lint: $(SOURCES)
	hlint $(SOURCES)

doc: $(SOURCES)
	haddock -h $(SOURCES) -o $(DOCS)

clean:
	rm -f bin/*.hi bin/*.o $(EXE)
