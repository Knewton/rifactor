default: build

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
	if [ -f node_modules/.bin/grunt ]; then node_modules/.bin/grunt clean ; fi

distclean: clean
	rm -rf node_modules bower_components

TMP := $(shell mktemp -d)

.PHONY: \
	build \
	clean \
	default \
	distclean \
	test
