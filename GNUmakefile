SOURCES=Main.hs
EXE=bin/LoadingPlanes

all: compile doc lint

compile: $(SOURCES)
	ghc --make -Wall $^ -outputdir bin -o $(EXE)

run: all
	$(EXE)

lint:
	hlint $(SOURCES)

doc:
	haddock -h $(SOURCES)

clean:
	rm -f bin/*.hi bin/*.o $(EXE)
