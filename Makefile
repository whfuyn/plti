default: tiny2

build:
	npx rescript build

tiny2: build
	node src/tiny2.bs.js

.PHONY: default build tiny2
