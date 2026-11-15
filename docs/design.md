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

## Reference limits

libfst's parallel writer (`fstWriterSetParallelMode`) is not used for the corpus. On arm64 CI
runners it intermittently writes value changes that libfst's own reader rejects (`fac: 2 clen:
120 (rc=-3)`), and on x86-64 a file that flushes and writes again at the same time made the
reader crash. Parallel mode changes how blocks are written, not their format.

## Module map

`src/` is one library, `fst`, whose only public module is `Fst` (`fst.mli`); the rest are hidden.

| Module | Role |
| --- | --- |
| `bin_reader` | Bounded big-endian and LEB128 reads over a string. Every error carries an offset, a file offset or, for a `~hier` reader, one into the decompressed hierarchy. |
| `error` | The error record and the internal exception `E`; `catch` turns it, and `Sys_error`, into `Error`. |
| `codec` | zlib and gzip (zipc), LZ4 (unlz4) and FastLZ, each with a size check against the codec's best ratio before anything is allocated. |
| `fastlz` | FastLZ decoder. |
| `header` | The header block. |
| `hier` | The hierarchy tag stream: `fold` (entries), `signals` (kind and length per handle, no names), `count_handles`. |
| `file` | The source (a string or a channel, behind a `guarded` reader that fails once closed), the block index of `fstReaderInit`, the cached `signals`, geometry, the time-table parser shared by every entry point. |
| `vc` | Value change blocks: chain-table parsing, the per-block traversal, `reader`/`step`, `start_at` (seeking) and `activity`. |
| `vcd` | VCD text on top of `Vc`: definitions, identifier codes, value spelling. |
| `fst` | The public module: wraps the others in `Err.catch` and states the API. |

## Seeking

`blocks` is the index: `File.index` reads 9 bytes at each block (type and length) when the file
is opened, and `Fst.blocks` then reads the first 16 bytes after the length for the block's start
and end times. `reader_at k` starts a `Vc.reader` at block `k`. A value change block's frame holds
the value of every handle except the string variables at its start, so the first step emits the
frame and, for the string variables, first runs a prepass: a second reader over blocks `0..k-1`
that selects only the string handles and keeps the last value of each.

Seeks share checkpoints. `File.t` keeps, for each set of selected string handles, an array
with the prepass state before every block, filled as seeks pass the boundaries: a seek to `k`
resumes from the nearest known boundary `j <= k` and reads blocks `j..k-1` only, so seeks in any
order read each earlier block at most once. A checkpoint holds the last value of each string
handle that has one (a persistent map, so boundaries share their strings), the size of the chain
length array and its negative entries (the stale lengths a block can inherit: a resumed prepass
must see what a continuous one does), and nothing else. A block that fails is not recorded, so
later seeks past it reread it and fail with the same error. The checkpoints belong to the handle,
are single-domain like the rest of it, and are dropped by `close`. The cost of a first seek is
that of reading every earlier block and decompressing the string handles' chains in it.

`aligned_block` reads time tables only, and decodes a block when its first time equals the
previous block's last.

What `reader_at k` guarantees is what libfst's time-limited read (`fstReaderSetLimitTimeRange`)
makes from block `k`: a `frame` call, a `time` call for the block's first time even if the
previous block ended on it, then the block's changes as a sequential read makes them. The
string variables, which libfst's frame lacks, are the addition: the prepass recovers them. This is
decision D2 (fallback B): `test/oracle` compares the calls with libfst's for every `k` whose
start time skips exactly the blocks before it.

There is one documented exception to `reader_at k` making the calls of the sequential suffix. A
chain table can hold a forward alias, one that names a later handle; libfst's writer never makes
one, so only a hand-built file does. Its negative length stays in a sequential reader's table and
a later block that lists the handle as unchanged resolves it. A seek does not carry that length
into block `k` (libfst's time-limited read does not either), so the handle's changes in `k` are
missing. `test/fixture/test_fixture.ml` asserts this, against libfst, as an exception, not as a
defect to fix.

## Select and packed

`select` is a predicate on handles, evaluated once per handle at the first step. An unselected
handle's chain is checked (its size still counts toward the block's declared memory, so which
blocks are rejected does not depend on the selection) but neither decompressed nor traversed.
`packed` replaces `sink.vector` for a changed vector that the file stores as 0 and 1 only: the
bytes are the file's bit-packed form, so no ASCII expansion happens. Frames, other vectors,
reals and strings go to the sink as before.

## Memory

- The file is read through a channel with a seek per read; nothing is mapped. A gzip-wrapped file
  is the exception: it is inflated whole when opened, and the channel closed.
- Opening keeps one record per block (`vc_blocks`), the header, the blackouts and the position of
  the hierarchy and geometry blocks. The hierarchy is not kept: `signals` keeps one `(kind, length)`
  per handle, and `fold_hierarchy` decompresses it again each call.
- A step holds one block: its bytes, the decoded time table, the frame, and a traversal buffer
  that grows to what the selected chains decompress to (never beyond the block's declared memory
  plus 66 bytes, and only as data arrives). Per-handle arrays (`scatter`, `headptr`,
  `remaining`) are sized by the handle count and reused across blocks, as is the traversal buffer.
- Seek checkpoints (see Seeking) are kept per handle until `close`: one small record per block
  boundary reached, plus the string values, which boundaries share.
- Sink arguments are views into those buffers, valid only during the call.

## libfst compatibility

The data API (`iter_changes`, `reader`, `Vcd`) matches libfst's values, ordering and VCD text for
every well-formed file; the corpus tests require byte identity. That is a stated decision: it
is what lets libfst be the oracle. It does not extend to libfst's handling of damaged files.
libfst continues past some damage and reads out of bounds past other; this library returns an
error for each, so a damaged trace cannot pass for a short or sparse one. The departures from
libfst's behaviour (not just from its crashes) are:

1. An invalid time-table trailer: libfst stops reading blocks silently; here it is `Malformed`.
2. An inconsistent chain table or chain sizes: libfst skips the block silently; here it is
   `Malformed`, naming the block.

Smaller choices of the same kind: a real whose length is not 8 and an alias to a handle that
does not exist are `Malformed`; a file with both a gzip and an LZ4 hierarchy block is
`Unsupported` where libfst takes the last.

## Robustness

Malformed input raises `Error.E` internally and returns `Error` at the API. Where libfst
aborts or reads out of bounds, this reader returns an error. It departs from libfst, which
carries on, in two places, so that a damaged file cannot look like a short or a sparse one:

- A block whose trailer gives an invalid time table: libfst `break`s out of the block loop
  without an error. Here it is `Malformed` from every entry point.
- A block whose chain table or chain sizes are inconsistent (an unparsable table, a chain
  shorter than its length, chains over the memory the block declares): libfst abandons the
  block (`goto block_err`) and goes on with the next, reporting nothing. Here it is `Malformed`
  naming the block, by its index among `blocks`. libfst's malformed-file tests allow a reader
  to reject such files, and three of them (`rvat_chain_table_overrun`,
  `uncompressed_chain_past_traversal`, `uncompressed_chain_shorter_than_its_varint`) now do.
  VCD output of well-formed files is unchanged.

`blocks` reads only 16 bytes per block, so the error appears when the block is decoded.

`Error.offset` says which coordinate it is: `` `File `` is an offset into the file, `` `Hierarchy ``
one into the decompressed hierarchy (which no file offset corresponds to), and `None` means no
position applies (an I/O error, a missing block).

Exceptions that are not errors in the input are not mapped: `Invalid_argument` for a block index
that is not one, and whatever a sink raises. A transient `Io` failure while reading the hierarchy
is not cached in `signals`; every other outcome is.

Every decoded size is checked against the codec's best ratio (zlib 1032:1, LZ4 and FastLZ
256:1) before any allocation, and the traversal buffer grows only as data arrives, so a
corrupt size field cannot cause a large allocation.

Times, file offsets and 64-bit header fields are `int64`; sizes inside a block are `int`, so on
32-bit hosts a block, the hierarchy or the traversal buffer must fit in a string
(`Sys.max_string_length`).
