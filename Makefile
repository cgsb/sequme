.PHONY: build doc test all setup setupclean install uninstall reinstall clean distclean


SETUP = ocaml setup.ml

all: build

setupclean:
	oasis setup-clean
	$(RM) myocamlbuild.ml
	$(RM) _tags
	$(RM) setup.ml

setup: setupclean
	oasis setup
	cat ocamlbuild_ocamldoc.ml >> myocamlbuild.ml
	cat custom_tags >> _tags

build: setup.data
	$(SETUP) -build $(BUILDFLAGS)

doc: setup.data build
	$(SETUP) -doc $(DOCFLAGS)

test: setup.data build
	$(SETUP) -test $(TESTFLAGS)


install: reinstall

uninstall: setup.data
	ocamlfind remove sequme
	$(SETUP) -uninstall $(UNINSTALLFLAGS)

reinstall: setup.data uninstall
	$(SETUP) -install $(REINSTALLFLAGS)

clean:
	rm -fr _build

distclean: clean
	$(RM) setup.data setup.log
	$(SETUP) -distclean $(DISTCLEANFLAGS)

