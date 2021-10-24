OCAMLOPT_FLAGS += -annot -bin-annot
bin_dir ?= /usr/local/bin

.PHONY: clean clean-extra install uninstall

all: bin-annot

bin-annot: bin_annot.ml
	ocamlfind ocamlopt $(OCAMLOPT_FLAGS) -linkpkg -package ocaml-compiler-libs.common $< -o $@

install: bin-annot
	install -D -t $(bin_dir) bin-annot

uninstall:
	$(RM) $(bin_dir)/bin-annot

clean:
	$(RM) bin_annot.annot bin_annot.cmi bin_annot.cmo bin_annot.cmt bin_annot.cmx bin_annot.o

clean-extra: clean
	$(RM) bin-annot
