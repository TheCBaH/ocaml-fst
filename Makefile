OPAM_EXEC = opam exec --
DUNE = $(OPAM_EXEC) dune
REF = _build/default/test/reference/fst_ref_a2.exe
FST2VCD = _build/default/bin/fst2vcd.exe
VERILATOR_DIR = _verilator
BENCH_DIR = _bench
BENCH = _build/default/bench/bench.exe
BENCH_CYCLES ?= 50000
MIXED_CYCLES ?= 200000

.PHONY: deps build test fmt fmt-check verilator-test corpus-digests ci release-test clean bench bench-trace

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

# A source tarball has no vendor/libfst submodule: builds a `git archive HEAD` in a temporary
# directory and runs @all @runtest there. The tests against the C reference (test/reference,
# test/corpus, test/meta, test_fixture) are skipped; test_unit, test_io, test_defects and
# test_cli run.
release-test:
	d=$$(mktemp -d /tmp/fst-release.XXXXXX) && git archive HEAD | tar -x -C "$$d" && \
	  $(DUNE) build --root "$$d" @all @runtest; rc=$$?; \
	  test $$rc -ne 0 || test ! -e "$$d/_build/default/test/reference/fst_ref_a2.exe" || rc=1; \
	  rm -rf "$$d"; exit $$rc

# Large Verilator traces (about 100 MB and 40 MB) for the benchmarks; needs Verilator.
bench-trace:
	mkdir -p $(BENCH_DIR)
	for m in 0 1; do \
	  verilator --cc --exe --build -j 2 --trace-fst -Wno-fatal --top-module top -GMODE=$$m \
	    -Mdir $(BENCH_DIR)/obj$$m $(CURDIR)/bench/verilator/top.sv $(CURDIR)/bench/verilator/main.cpp; \
	done
	$(BENCH_DIR)/obj0/Vtop $(BENCH_DIR)/random.fst $(BENCH_CYCLES) 500
	$(BENCH_DIR)/obj1/Vtop $(BENCH_DIR)/mixed.fst $(MIXED_CYCLES) 2000
	rm -rf $(BENCH_DIR)/obj0 $(BENCH_DIR)/obj1

# libfst's reader again, built with -O2 (the corpus build uses -O1), as a faster baseline.
$(BENCH_DIR)/fst_ref_o2: test/reference/fst_ref.c
	mkdir -p $(BENCH_DIR)
	$(CC) -O2 -D_GNU_SOURCE -D_FILE_OFFSET_BITS=64 -DHAVE_FSEEKO -DHAVE_LIBPTHREAD \
	  -DFST_WRITER_PARALLEL '-DVARIANT="a2"' -Ivendor/libfst/src -o $@ $< \
	  vendor/libfst/src/fstapi.c vendor/libfst/src/lz4.c vendor/libfst/src/fastlz.c -lz -lpthread -lm

# Timings and allocation against libfst on the traces: results in $(BENCH_DIR)/results.csv.
# Not part of ci. Needs Verilator for the traces.
bench: bench-trace $(BENCH_DIR)/fst_ref_o2
	$(DUNE) build bench/bench.exe $(REF)
	rm -f $(BENCH_DIR)/results.csv
	for t in random mixed; do \
	  for g in conv sparse; do \
	    $(BENCH) $$g --csv $(BENCH_DIR)/results.csv --tag $$t --trace $(BENCH_DIR)/$$t.fst \
	      --ref $(REF) --ref $(BENCH_DIR)/fst_ref_o2; \
	  done; \
	done
	for g in seek hier micro; do \
	  $(BENCH) $$g --csv $(BENCH_DIR)/results.csv --trace $(BENCH_DIR)/random.fst \
	    --ref $(REF) --ref $(BENCH_DIR)/fst_ref_o2; \
	done

clean:
	$(DUNE) clean
	rm -rf $(VERILATOR_DIR)
