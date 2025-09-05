OCAMLOPT = ocamlopt
OCAMLC = ocamlc
SOURCES = util.ml lexer.ml oli.ml
OBJS = $(SOURCES:.ml=.cmx)

oli: $(OBJS)
	$(OCAMLOPT) -o oli unix.cmxa $(OBJS)

%.cmx: %.ml
	$(OCAMLOPT) -c $<

clean:
	rm -f *.cmi *.cmo *.cmx *.o oli *.s *.tmp

install: oli
	cp oli /usr/local/bin/

.PHONY: clean install oli
