default: .cabal-sandbox/bin/rifactor

.cabal-sandbox:
	cabal sandbox init

.cabal-sandbox/bin/rifactor: | .cabal-sandbox
	cabal install || cabal install --force-reinstalls

# JS: INSTALL NPM DEPENDENCIES
node_modules: package.json
	npm install

# JS: INSTALL BOWER DEPENDENCIES
bower_components: node_modules bower.json
	node_modules/.bin/bower --allow-root install

# JS: BUILD
build: node_modules bower_components
	node_modules/.bin/grunt build

# JS: TEST
test: node_modules bower_components
	node_modules/.bin/grunt test

clean:
	cabal clean
	if [ -f node_modules/.bin/grunt ]; then node_modules/.bin/grunt clean ; fi

distclean: clean
	rm -rf .cabal-sandbox
	rm -rf node_modules bower_components

.PHONY: \
	build \
	clean \
	default \
	distclean \
	test
