# Copyright   : (c) 2015 Knewton, Inc <se@knewton.com>
#               (c) 2015 Tim Dysinger <tim@dysinger.net> (contributor)
# License     : Apache 2.0 http://opensource.org/licenses/Apache-2.0
# Maintainer  : Tim Dysinger <tim@dysinger.net>

default: .cabal-sandbox/bin/rifactor

.cabal-sandbox:
	@cabal sandbox init
	@cabal sandbox add-source ../../dysinger/amazonka/core
	@cabal sandbox add-source ../../dysinger/amazonka/amazonka
	@cabal sandbox add-source ../../dysinger/amazonka/amazonka-ec2

.cabal-sandbox/bin/rifactor: | .cabal-sandbox
	@cabal install

clean:
	@cabal clean

distclean: clean
	@rm -rf .cabal-sandbox dist

.PHONY: default clean distclean
