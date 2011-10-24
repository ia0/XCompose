.PHONY: all clean

all: XCompose

SOURCES = $(wildcard *.hs)
GENERATED = XCompose Main

XCompose: Main
	./$^ > $@

Main: $(SOURCES)
	ghc -Wall --make $@

clean:
	rm $(GENERATED) *.hi *.o

