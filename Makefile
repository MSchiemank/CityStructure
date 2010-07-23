# Configuration
COMPILER=ghc
OPTIONS=--make -O2 -fforce-recomp -Wall -threaded
BINARIES=CityStruct


# Build the main project
CityStruct: CityStruct.hs
	$(COMPILER) $(OPTIONS) $+ -o CityStruct


# Remove build files and generated binaries.
clean:
	rm -f *.o *.hi $(BINARIES)
	

