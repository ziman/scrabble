.PHONY: all clean

all: www/app.js

clean:
	-rm -f index.js index.js.app www/app.js

index.js: src/**/*.purs src/*.purs
	spago bundle-app --source-maps

www/app.js: index.js
	browserify $< -o $@
