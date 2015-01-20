# Copyright   : (c) 2015 Knewton, Inc <se@knewton.com>
#               (c) 2015 Tim Dysinger <tim@dysinger.net> (contributor)
# License     : Apache 2.0 http://opensource.org/licenses/Apache-2.0
# Maintainer  : Tim Dysinger <tim@dysinger.net>

default: .cabal-sandbox/bin/rifactor

.cabal-sandbox:
	@cabal sandbox init

build: | .cabal-sandbox
	@cabal install --enable-tests

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
