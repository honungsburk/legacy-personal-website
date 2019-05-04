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

publish:
	@ git stash 				# Stash uncommited changes
	@ git checkout development  # Make sure we are on the correct branch
	@ stack exec -- blog clean  # Make a clean build
	@ stack exec -- blog build
	@ git fetch --all 
	@ git checkout master 
	# we use rsync to auto remove deleted files
	@ rsync -a --filter='P _site/'      \
			 --filter='P _cache/'     \
			 --filter='P .git/'       \
			 --filter='P .gitignore'  \
			 --filter='P .stack-work' \
			 --delete-excluded        \
			 _site/ .
	@ git add -A
	@ git commit -m "Publish."
	@ git push origin
	@ git checkout development 	# cleanup
	@ git stash pop

.PHONY: all build watch check clean ghcid blog publish
