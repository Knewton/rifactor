# Module      : Rifactor.Plan
# Copyright   : (c) 2015 Knewton, Inc <se@knewton.com>
#               (c) 2015 Tim Dysinger <tim@dysinger.net> (contributor)
# License     : Apache 2.0 http://opensource.org/licenses/Apache-2.0
# Maintainer  : Tim Dysinger <tim@dysinger.net>
# Stability   : experimental
# Portability : non-portable (GHC extensions)

default: .cabal-sandbox/bin/rifactor

.cabal-sandbox:
	@cabal sandbox init

.cabal-sandbox/bin/rifactor: | .cabal-sandbox
	@cabal update
	@cabal install \
		--enable-executable-stripping \
		--enable-split-objs \
		--force-reinstalls

clean:
	@cabal clean

distclean: clean
	@rm -rf .cabal-sandbox dist

.PHONY: default clean distclean
