# Copyright   : (c) 2015 Knewton, Inc <se@knewton.com>
#               (c) 2015 Tim Dysinger <tim@dysinger.net> (contributor)
# License     : Apache 2.0 http://opensource.org/licenses/Apache-2.0
# Maintainer  : Tim Dysinger <tim@dysinger.net>

default: .cabal-sandbox/bin/rifactor

.cabal-sandbox:
	@cabal sandbox init

.cabal-sandbox/src/amazonka: | .cabal-sandbox
	@git clone --branch=develop --depth=1 git://github.com/dysinger/amazonka ./.cabal-sandbox/src/amazonka
	@cabal sandbox add-source ./.cabal-sandbox/src/amazonka/core
	@cabal sandbox add-source ./.cabal-sandbox/src/amazonka/amazonka
	@cabal sandbox add-source ./.cabal-sandbox/src/amazonka/amazonka-ec2

build: | .cabal-sandbox/src/amazonka
	@cabal install --enable-tests --enable-benchmarks

.cabal-sandbox/bin/rifactor: | build
.cabal-sandbox/bin/test: | build

doc:
	@cabal haddock --hyperlink-source

test: | .cabal-sandbox/bin/test
	@cabal test

clean:
	@cabal clean

distclean: clean
	@rm -rf .cabal-sandbox dist

.PHONY: default doc test clean distclean
