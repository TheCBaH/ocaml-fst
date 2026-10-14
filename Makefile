OPAM_EXEC = opam exec --
DUNE = $(OPAM_EXEC) dune
REF = _build/default/test/reference/fst_ref_a2.exe
FST2VCD = _build/default/bin/fst2vcd.exe
VERILATOR_DIR = _verilator

.PHONY: deps build test fmt fmt-check verilator-test corpus-digests ci clean

deps:
	opam install . --deps-only --with-test -y

build:
	$(DUNE) build @all

# Needs the vendor/libfst submodule: git submodule update --init vendor/libfst
test:
	$(DUNE) build @runtest

fmt:
	$(DUNE) build @fmt --auto-promote

fmt-check:
	$(DUNE) build @fmt

# Accepts the digests of the golden VCD files after a deliberate change to the corpus.
corpus-digests:
	-$(DUNE) build @test/corpus/runtest --auto-promote

# A real Verilator simulation traced with --trace-fst, converted by fst2vcd and by libfst.
verilator-test:
	$(DUNE) build $(REF) $(FST2VCD)
	rm -rf $(VERILATOR_DIR) && mkdir -p $(VERILATOR_DIR)
	verilator --cc --exe --build -j 2 --trace-fst -Wno-fatal --top-module top \
	  -Mdir $(VERILATOR_DIR)/obj $(CURDIR)/test/verilator/top.sv $(CURDIR)/test/verilator/main.cpp
	$(VERILATOR_DIR)/obj/Vtop $(VERILATOR_DIR)/trace.fst
	set -e; for e in "" -e; do \
	  $(REF) vcd $$e $(VERILATOR_DIR)/trace.fst $(VERILATOR_DIR)/expected$$e.vcd; \
	  $(FST2VCD) $$e $(VERILATOR_DIR)/trace.fst -o $(VERILATOR_DIR)/actual$$e.vcd; \
	  cmp $(VERILATOR_DIR)/expected$$e.vcd $(VERILATOR_DIR)/actual$$e.vcd; \
	done
	@wc -l $(VERILATOR_DIR)/expected.vcd
	rm -rf $(VERILATOR_DIR)

ci: fmt-check build test

clean:
	$(DUNE) clean
	rm -rf $(VERILATOR_DIR)
