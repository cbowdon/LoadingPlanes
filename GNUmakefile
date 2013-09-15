SOURCES=Main.hs
EXE=bin/LoadingPlanes

all: $(SOURCES)
	ghc --make -Wall $^ -outputdir bin -o $(EXE)

run: all
	$(EXE)

lint:
	hlint *.hs

clean:
	rm -f bin/*.hi bin/*.o $(EXE)
