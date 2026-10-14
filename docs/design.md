# Design

## Reference

FST has no official specification. This reader follows libfst's `src/fstapi.c` (the
`vendor/libfst` submodule), in particular `fstReaderInit` (block index), `fstReaderProcessHier`
(hierarchy and VCD definitions) and `fstReaderIterBlocks2` (value changes). Secondary sources:
Tim Hutt's draft [FST specification](https://blog.timhutt.co.uk/fst_spec/) and the Rust
[fst-reader](https://github.com/ekiwi/fst-reader).

Where the sources disagree, `fstapi.c` decides:

- The compressed wrapper (block 254) and the HIER block (4) are gzip members (`gzdopen "wb4"`),
  not raw zlib as `doc/block_format.txt` says. Like `gzread`, data without the gzip magic is read
  as stored.
- The header's date field is 119 bytes (`FST_HDR_DATE_SIZE`): the specification's 26 date bytes
  plus 93 reserved ones are one NUL-padded field.
- In a multi-bit value change, a clear low bit of the leading varint means bit-packed binary
  (MSB first) and a set bit means one ASCII character per bit, the opposite of the
  specification's `all_binary`.
- Type 1 and type 5 value change blocks share one chain table encoding (a zero varint followed
  by a 1-based alias target); type 8 uses signed varints for aliases.
- Real values with a clear low bit carry one byte whose bits, as eight `'0'`/`'1'`
  characters, are reinterpreted as a double, as libfst does.

## Output identity

`Fst.Vcd` produces libfst's `fst2vcd` output byte for byte, so libfst is the test oracle
(`test/reference`). That means reproducing its behaviour, not just the format:

- Variables due at the same time index are chained LIFO, which fixes the order of changes
  within a time step.
- The first block's initial values (the frame) are written only when the block starts before
  its first change, and before any `#` line when it starts at time 0.
- Chain table lengths persist across blocks, so a variable that was a dynamic alias in one
  block and has no changes in the next is resolved as the alias again.
- Hierarchy lengths (not geometry) give each variable's width; ports are `(n - 2) / 3` wide and
  their values use `p`.
- Names are kept up to 69632 bytes, empty scope names become `$unnamed_scope_N`, and empty
  attribute names `""`.
- Reals print with `%.16g`, including glibc's `nan`, `-nan`, `inf`.
- `FST_EV_REG` and `FST_EV_TIME` enum attributes fall outside libfst's name table (it reads
  past the end); they are named `reg` and `time` here and kept out of the corpus.

## Robustness

Malformed input raises `Error.E` internally and returns `Error` at the API. Where libfst
silently abandons a block (`goto block_err`: an inconsistent chain table or a traversal buffer
overrun), so does this reader; where libfst aborts or reads out of bounds, this reader returns
an error. Every decoded size is checked against the codec's best ratio (zlib 1032:1, LZ4 and
FastLZ 256:1) before any allocation, and the traversal buffer grows only as data arrives, so a
corrupt size field cannot cause a large allocation.

Times, file offsets and 64-bit header fields are `int64`; sizes inside a block are `int`, so on
32-bit hosts a block, the hierarchy or the traversal buffer must fit in a string
(`Sys.max_string_length`).
