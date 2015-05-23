FILE=Main
PACKAGES=-package-db=`find .cabal-sandbox/ -type d -name "x86_64-linux-ghc-7.*-packages.conf.d"`
OPTIONS=-O2 -threaded

all:
	ghc $(PACKAGES) $(OPTIONS) --make $(FILE)

test:
	watch -d -n 1 'ghc $(PACKAGES) -fno-code $(FILE)'

clean:
	rm -f *.hi *.o

setup:
	cabal sandbox init
	cabal update
	cabal install gloss
