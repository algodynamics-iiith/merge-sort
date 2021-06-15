shuffle: 
	elm make src/Shuffle.elm --output dist/js/shuffle.js

shuffle-sorted: 
	elm make src/ShuffleSorted.elm --output dist/js/shuffleSorted.js

merge-strategy:
	elm make src/MergeStrategy.elm --output dist/js/mergeStrategy.js

merge-algo:
	elm make src/MergeAlgo.elm --output dist/js/mergeAlgo.js

merge-sublists:
	elm make src/MergeSublists.elm --output dist/js/mergeSublists.js

ms-recursive:
	elm make src/MergeSortRecursive.elm --output dist/js/mergeSortRecursive.js

ms-arbitrary:
	elm make src/MsArbitrary.elm --output dist/js/mergeSortArbitrary.js

webpack:
	npm run build

css:
	cp -r src/css dist/

html:
	cp -r src/html/* dist/

js:
	mkdir -p dist/js

dist:
	mkdir -p dist

init: dist js css html

all: init webpack shuffle shuffle-sorted merge-strategy merge-algo merge-sublists ms-recursive ms-arbitrary

elm: shuffle shuffle-sorted merge-strategy merge-algo merge-sublists ms-recursive ms-arbitrary
