.PHONY: all

all: www/app.js

index.js: src/*.purs
	spago bundle-app --source-maps

www/app.js: index.js
	browserify $< -o $@
