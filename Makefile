BASENAME := koog
VERSION := 1.1
URL := http://koog.contextlogger.org/
REPO_URL := git://github.com/contextlogger/koog.git

default : install

-include local.mk

install : install-sh

# cli-shell.sh is implemented not to depend on its location,
# so symlinking is fine, there is no location confusion.
install-sh :
	-mkdir -p $(HOME)/bin
	ln -sf `pwd`/cli-shell.sh $(HOME)/bin/koog

info.rkt : Makefile
	echo "#lang setup/infotab" > $@
	echo ";; generated file, do not edit" >> $@
	echo "(define name \"$(BASENAME)\")" >> $@
	echo "(define version \"$(VERSION)\")" >> $@
	echo '(define blurb (list "A mixed-code generator library and command-line tool."))' >> $@
	echo "(define url \"$(URL)\")" >> $@
	echo "(define repo-url \"$(REPO_URL)\")" >> $@
	echo "(define scribblings '((\"manual.scrbl\" ())))" >> $@
	echo '(define primary-file "koog.ss")' >> $@

SOURCES = cli-shell.sh cli.ss emacs/koog.el index.html info.rkt INSTALL koog.ss LICENSE Makefile manual.scrbl runtime.ss tools/adjust-scribble.rb util.ss vim/koog.vimrc
SOURCES_WITH_DIR = $(patsubst %, $(BASENAME)/%, $(SOURCES))

TARBALL := dist/$(BASENAME)-$(VERSION).tar.gz

# Scribble seems immature. Some of the documented options have no effect.
# Instead of fighting with the tool we just patch the output.
man : info.rkt
	-rm -r manual
	scribble --html --dest manual manual.scrbl
	ruby tools/adjust-scribble.rb manual manual.scrbl ../index.html

tarball : man
	-mkdir -p dist
	tar -c -z -C .. -v -f $(TARBALL) $(SOURCES_WITH_DIR) koog/manual

# This is pointless when hosted on GitHub, as there is an interface
# for browsing the repo.
src-web-dir :
	mkdir web/src
	rsync -a --exclude '*.html' --exclude '*.scrbl' --exclude INSTALL --exclude LICENSE --exclude Makefile --exclude '*.rb' $(SOURCES) web/src/

MKINDEX := ../tools/bin/make-index-page.rb

.PHONY : web

web :
	-rm -r web
	mkdir web
	cp *.txt index.html style.css web/
	rsync -av manual dist web/
	$(MKINDEX) web/dist
	chmod -R a+rX web
	tidy -utf8 -eq web/index.html

HTDOCS := ../contextlogger.github.com
PAGEPATH := koog
PAGEHOME := $(HTDOCS)/$(PAGEPATH)

release :
	-mkdir -p $(PAGEHOME)
	rsync -av --delete web/ $(PAGEHOME)/

upload :
	cd $(HTDOCS) && git add $(PAGEPATH) && git commit -a -m updates && git push

all : tarball web
