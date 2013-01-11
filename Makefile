.PHONY: build doc test all install uninstall reinstall clean distclean

SETUP = ocaml setup.ml

build: setup.data
	$(SETUP) -build $(BUILDFLAGS)

all: build

doc: setup.data build
	$(SETUP) -doc $(DOCFLAGS)

test: setup.data build
	$(SETUP) -test $(TESTFLAGS)

uninstall: setup.data
	$(SETUP) -uninstall $(UNINSTALLFLAGS)

reinstall: setup.data uninstall
	ocamlfind remove sequme
	$(SETUP) -install $(REINSTALLFLAGS)

install: reinstall

setup.ml: _oasis
	oasis setup -setup-update dynamic

setup.data: setup.ml
	$(SETUP) -configure $(CONFIGUREFLAGS)

configure: setup.data

clean:
	rm -fr _build

distclean:
	$(RM) setup.data setup.log
	$(RM) configure
	$(RM) src/lib/META
	$(RM) src/lib/doclib.odocl
	$(RM) src/lib/sequme.mllib
	$(RM) TAGS
	$(SETUP) -distclean $(DISTCLEANFLAGS)
	$(RM) setup.ml

TAGS_INCLUDE=-I $(shell ocamlfind query sexplib.syntax) -I $(shell ocamlfind query type_conv)
TAGS_LINK=-pa pa_type_conv.cma -pa pa_sexp_conv.cma

TAGS:
	otags $(TAGS_INCLUDE) $(TAGS_LINK) -o TAGS `find src -regex ".*\.ml"`
