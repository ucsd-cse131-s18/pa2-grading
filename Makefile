UNAME := $(shell uname)
ifeq ($(UNAME), Linux)
  FORMAT=aout
else
ifeq ($(UNAME), Darwin)
  FORMAT=macho
endif
endif
ifneq (,$(findstring Microsoft,$(shell cat /proc/version 2> /dev/null)))
  OCAMLOPT=-ocamlopt "ocamlopt -no-alias-deps"
endif

PKGS=oUnit,extlib,unix,sexplib

BUILD=corebuild -r -use-ocamlfind -pkg $(PKGS) $(OCAMLOPT)

main: main.ml compile.ml runner.ml parser.ml
	$(BUILD) main.native
	mv main.native main

test: compile.ml runner.ml test.ml parser.ml
	mkdir -p output
	$(BUILD) test.native
	mv test.native test

output/%.run: output/%.o main.c
	clang -g -m32 -mstackrealign -o $@ main.c $<

output/%.o: output/%.s
	nasm -f $(FORMAT) -o $@ $<

output/%.s: input/%.ana main
	mkdir -p output
	./main $< > $@

clean:
	rm -rf output/*.o output/*.s output/*.dSYM output/*.run *.log
	rm -rf _build/
	rm -f main test
