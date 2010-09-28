BASENAME := koog
VERSION := 1.0
URL := http://koog.contextlogger.org/

default : install-sh

-include local.mk

# cli-shell.sh is implemented not to depend on its location,
# so symlinking is fine, there is no location confusion.
install-sh :
	-mkdir -p $(HOME)/bin
	ln -sf `pwd`/cli-shell.sh $(HOME)/bin/koog

info.rkt : Makefile
	echo "#lang setup/infotab" > $@
	echo "(define name \"$(BASENAME)\")" >> $@
	echo "(define version \"$(VERSION)\")" >> $@
	echo '(define blurb (list "A mixed-code generator library and command-line tool."))' >> $@
	echo "(define url \"$(URL)\")" >> $@
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
	#cp -a style.css manual/scribble-style.css

tarball : man
	-mkdir -p dist
	tar -c -z -C .. -v -f $(TARBALL) $(SOURCES_WITH_DIR) koog/manual

homepage :
	-rm -r web
	mkdir web
	cp INSTALL LICENSE index.html style.css web/
	rsync -av manual dist web/ 
	mkdir web/src
	rsync -a --exclude '*.html' --exclude '*.scrbl' --exclude INSTALL --exclude LICENSE --exclude Makefile --exclude '*.rb' $(SOURCES) web/src/
	chmod -R a+rX web
	tidy -utf8 -eq web/index.html

all : tarball homepage
