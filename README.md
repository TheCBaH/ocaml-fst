# fst

[![ci](https://github.com/TheCBaH/ocaml-fst/actions/workflows/ci.yml/badge.svg?branch=main)](https://github.com/TheCBaH/ocaml-fst/actions/workflows/ci.yml?query=branch%3Amain)

A native OCaml reader for [FST](https://blog.timhutt.co.uk/fst_spec/), the compressed waveform
format of [GTKWave](https://github.com/gtkwave/gtkwave) that [Verilator](https://verilator.org)
writes with `--trace-fst`, and an `fst2vcd` that converts it to VCD byte for byte as libfst's
`fst2vcd` does.

- **Complete**: all value change block types (plain, dynamic alias, dynamic alias 2), zlib,
  FastLZ and LZ4 value changes, gzip, LZ4 and double-LZ4 hierarchies, gzip-wrapped files,
  blackouts, attributes, aliases, reals and strings.
- **Pure OCaml**: zlib and gzip through [zipc](https://erratique.ch/software/zipc), LZ4 through
  [unlz4](https://github.com/TheCBaH/ocaml-unlz4), FastLZ here. No C stubs.
- **Checked**: malformed input yields a structured `Fst.Error.t`, never an exception; sizes are
  bounded before anything is allocated.

See [docs/design.md](docs/design.md) for the sources and the behaviours reproduced.

## Usage

```
fst2vcd trace.fst -o trace.vcd      # -e: FST attributes as $attrbegin, like fst2vcd -e
```

```ocaml
match Fst.open_file "trace.fst" with
| Error e -> prerr_endline (Fst.Error.to_string e)
| Ok t ->
    Fst.Vcd.write t stdout |> Result.iter_error (fun e -> prerr_endline (Fst.Error.to_string e));
    Fst.close t
```

`Fst.fold_hierarchy` walks scopes and variables, and `Fst.iter_changes` streams value changes
in time order to a callback record.

## Development

Use the [devcontainer](https://github.com/TheCBaH/ocaml-fst/tree/main/.devcontainer);
[CI](https://github.com/TheCBaH/ocaml-fst/actions/workflows/ci.yml?query=branch%3Amain) runs every
leg inside the same image (OCaml 4.14 and 5.4 on amd64 and arm64, 4.14 on i386 and armv7).

| Command | What it does |
| --- | --- |
| `make deps` | Install the opam dependencies (unlz4 is pinned by `fst.opam`) |
| `make ci` | Formatting check, build, tests |
| `make test` | Unit tests, corpus conversions, robustness |
| `make verilator-test` | Convert a real Verilator `--trace-fst` trace and compare with libfst |
| `make corpus-digests` | Accept new digests after a deliberate corpus change |

The tests convert a corpus that [`test/reference/fst_ref.c`](test/reference/fst_ref.c) writes at
build time with the pinned [libfst](https://github.com/gtkwave/libfst) (`vendor/libfst`, so run
`git submodule update --init`), and compare each result with the VCD libfst's own reader makes
of the same file.

## License

MIT. libfst (`vendor/libfst`, test only) is MIT licensed.
