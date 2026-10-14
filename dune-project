(lang dune 3.21)

(name fst)

(using directory-targets 0.1)

(generate_opam_files true)

(license MIT)

(authors "Vladimir Silyaev")

(maintainers "Vladimir Silyaev")

(source
 (github TheCBaH/ocaml-fst))

(package
 (name fst)
 (synopsis "Native OCaml FST waveform reader and FST to VCD converter")
 (description
  "Reads FST, the compressed waveform format of GTKWave written by Verilator and other simulators, and converts it to VCD byte-for-byte as libfst's fst2vcd does. zlib, gzip, LZ4 and FastLZ sections are decoded in OCaml, without C stubs.")
 (depends
  (ocaml
   (>= 4.14))
  (zipc
   (>= 0.2.0))
  unlz4))
