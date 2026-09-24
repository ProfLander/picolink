SHELL=/usr/bin/env bash

PACKAGE-NAME=picolink

install:
	raco pkg install --deps search-auto --link ${PWD}/${PACKAGE-NAME}-{lib,lang,backend,test,doc} $(PWD)/$(PACKAGE-NAME)

uninstall:
	raco pkg uninstall $(PACKAGE-NAME)-{lib,lang,backend,test,doc} $(PACKAGE-NAME)

build:
	raco setup --no-docs --pkgs $(PACKAGE-NAME)-lib

build-docs:
	raco setup --no-launcher --no-foreign-libs --no-info-domain --no-pkg-deps \
	--no-install --no-post-install --pkgs $(PACKAGE-NAME)-doc

build-standalone-docs:
	scribble +m --redirect-main http://pkg-build.racket-lang.org/doc/ --htmls \
	--dest ./docs ./picolink-doc/scribblings/picolink.scrbl

build-all:
	raco setup $(DEPS-FLAGS) --pkgs $(PACKAGE-NAME)-{lib,lang,backend,test,doc} $(PACKAGE-NAME)

clean:
	raco setup --fast-clean --pkgs $(PACKAGE-NAME)-{lib,lang,backend,test,doc}

test:
	raco test -exp $(PACKAGE-NAME)-{lib,lang,backend,test,doc}

.PHONY: install remove
