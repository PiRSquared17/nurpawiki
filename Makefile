
FILES=version.ml config.ml types.ml util.ml database.ml services.ml privileges.ml html_util.ml session.ml user_editor.ml page_revisions.ml nurpawiki.ml scheduler.ml history.ml about.ml

CAMLC = ocamlfind ocamlc -thread -g $(LIB)
CAMLOPT = ocamlfind ocamlopt -thread  $(LIB)
CAMLDOC = ocamlfind ocamldoc $(LIB)
CAMLDEP = ocamlfind ocamldep
OCSIGENREP = `ocamlfind query ocsigen`
#OCSIGENREP = ../ocsigen/lib
 # ^ pour l'instant
LIB = -package threads,netstring,calendar,extlib,postgresql,lwt -I $(OCSIGENREP)
PP = -pp "camlp4o $(OCSIGENREP)/xhtmlsyntax.cma"

OBJS = $(FILES:.ml=.cmo)

CMA = nurpawiki.cma

all: $(CMA) META

$(CMA): $(OBJS)
	$(CAMLC) -a -o $(CMA) $(OBJS)

.SUFFIXES:
.SUFFIXES: .ml .mli .cmo .cmi .cmx

.PHONY: doc install

NWIKI_VER=$(shell cat VERSION)
version.ml:version.ml.in VERSION
	echo $(NWIKI_VER)
	cat version.ml.in | \
	    sed -e "s|%_NURPAWIKI_VERSION_%|$(NWIKI_VER)|g" > version.ml

META:META.in VERSION
	cat META.in | \
	    sed -e "s|%_NURPAWIKI_VERSION_%|$(NWIKI_VER)|g" > META

.ml.cmo:
	$(CAMLC) $(PP) -c $<

.mli.cmi:
	$(CAMLC) -c $<
.ml.cmx:
	$(CAMLOPT) $(PP) -c $<

doc:
#	$(CAMLDOC) -d doc -html db.mli

clean:
	-rm -f *.cm[ioxa] *~ $(NAME)

install:
	ocamlfind install nurpawiki META $(CMA)

depend:
	$(CAMLDEP) $(PP) $(LIB) $(FILES:.ml=.mli) $(FILES) > .depend

