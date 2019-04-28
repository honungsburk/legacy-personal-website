all: build

build:
	@stack upgrade
	@stack update
	@stack build
	@stack exec -- blog build

watch: build
	@stack exec -- blog watch --port 4000

blog: clean
	@stack exec -- blog build

check: build
	@stack exec -- blog check

clean:
	@stack exec -- blog clean
	@stack clean

ghcid:
	@ ghcid --command "stack ghci blog:exe:blog --ghci-options=-fno-code"

.PHONY: all build watch check clean ghcid blog
