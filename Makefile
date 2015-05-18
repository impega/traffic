FILE=Main
PACKAGES=-package-db=.cabal-sandbox/x86_64-linux-ghc-7.8.4-packages.conf.d/
OPTIONS=-O2 -threaded

all:
	ghc $(PACKAGES) $(OPTIONS) --make $(FILE)

test:
	watch -d -n 1 'ghc $(PACKAGES) -fno-code $(FILE)'


