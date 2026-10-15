/* The test corpus, written with libfst (vendor/libfst), and the VCD that libfst's own reader
   makes of each file, as GTKWave's fst2vcd does. The value change block type is a compile-time
   choice of libfst's writer, so this is built three times (VARIANT a2, a1, a0: dynamic alias 2,
   dynamic alias, none).

     fst_ref corpus DIR           write DIR/VARIANT_*.fst, .vcd and .ext.vcd (-e)
     fst_ref vcd [-e] FST VCD     convert one file */

#include <errno.h>
#include <math.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/stat.h>

#include "fstapi.h"

#ifndef VARIANT
#define VARIANT "a2"
#endif

static int to_vcd(const char *fst, const char *vcd, int extensions)
{
    fstReaderContext *xc = fstReaderOpen(fst);
    FILE *fv;

    if (!xc) {
        fprintf(stderr, "fst_ref: cannot open %s\n", fst);
        return 1;
    }
    fv = fopen(vcd, "wb");
    if (!fv) {
        perror(vcd);
        fstReaderClose(xc);
        return 1;
    }
    fstReaderSetVcdExtensions(xc, extensions);
    if (!fstReaderProcessHier(xc, fv)) {
        fprintf(stderr, "fst_ref: no hierarchy in %s\n", fst);
        fclose(fv);
        fstReaderClose(xc);
        return 1;
    }
    fstReaderSetFacProcessMaskAll(xc);
    fstReaderIterBlocks(xc, NULL, NULL, fv);
    fstReaderClose(xc);
    fclose(fv);
    return 0;
}

/* Deterministic pseudo-random values. */
static unsigned long long seed = 1;
static unsigned rnd(void)
{
    seed = seed * 6364136223846793005ULL + 1442695040888963407ULL;
    return (unsigned)(seed >> 33);
}

static void bits(char *buf, int n, const char *alphabet)
{
    int k = (int)strlen(alphabet);
    for (int i = 0; i < n; i++)
        buf[i] = alphabet[rnd() % k];
    buf[n] = 0;
}

static void real(fstWriterContext *c, fstHandle h, double d)
{
    fstWriterEmitValueChange(c, h, &d);
}

/* Every variable kind, attribute and scope type a writer can produce, two value change blocks,
   blackouts, and variables that are equal in one block and not the next. */
static void kitchen(fstWriterContext *c)
{
    static const char states[] = "01xzhuwl-?XZHUWL";
    static const unsigned char text[] = "a b\n\t\"q\"\\?'\a\b\f\r\v\x01\x7f\xff~";
    char buf[256];
    fstHandle clk, st, b8, b13, wide, r, sr, rt, in, par, ev, s, port, d1, d2, d3, n1, never, gv;
    double specials[] = {0.0, -0.0, 1.0 / 3.0, -2.5e-300, 1e300, NAN, -NAN, INFINITY, -INFINITY};

    fstWriterSetComment(c, "kitchen sink");
    fstWriterSetEnvVar(c, "FST_REF=1");
    fstWriterSetScope(c, FST_ST_VCD_MODULE, "top", "top_component");
    fstWriterSetSourceStem(c, "/src/top.sv", 10, 0);
    fstWriterSetSourceInstantiationStem(c, "/src/tb.sv", 20, 0);
    clk = fstWriterCreateVar(c, FST_VT_VCD_WIRE, FST_VD_INPUT, 1, "clk", 0);
    st = fstWriterCreateVar(c, FST_VT_VCD_REG, FST_VD_IMPLICIT, 1, "states", 0);
    b8 = fstWriterCreateVar(c, FST_VT_SV_LOGIC, FST_VD_OUTPUT, 8, "b8[7:0]", 0);
    b13 = fstWriterCreateVar(c, FST_VT_VCD_REG, FST_VD_INOUT, 13, "b13", 0);
    fstWriterSetAttrBegin(c, FST_AT_ARRAY, FST_AR_PACKED, "arr", 4);
    wide = fstWriterCreateVar(c, FST_VT_SV_BIT, FST_VD_BUFFER, 130, "wide", 0);
    fstWriterSetAttrEnd(c);
    r = fstWriterCreateVar(c, FST_VT_VCD_REAL, FST_VD_IMPLICIT, 8, "r", 0);
    sr = fstWriterCreateVar(c, FST_VT_SV_SHORTREAL, FST_VD_IMPLICIT, 4, "sr", 0);
    rt = fstWriterCreateVar(c, FST_VT_VCD_REALTIME, FST_VD_IMPLICIT, 8, "rt", 0);
    fstWriterSetAttrBegin(c, FST_AT_ENUM, FST_EV_SV_UNSIGNED_INT, "e", 7);
    in = fstWriterCreateVar(c, FST_VT_VCD_INTEGER, FST_VD_LINKAGE, 32, "i", 0);
    fstWriterSetAttrEnd(c);
    par = fstWriterCreateVar(c, FST_VT_VCD_PARAMETER, FST_VD_IMPLICIT, 4, "P", 0);
    ev = fstWriterCreateVar(c, FST_VT_VCD_EVENT, FST_VD_IMPLICIT, 1, "ev", 0);
    s = fstWriterCreateVar(c, FST_VT_GEN_STRING, FST_VD_IMPLICIT, 0, "msg", 0);
    port = fstWriterCreateVar(c, FST_VT_VCD_PORT, FST_VD_INPUT, 3 * 4 + 2, "p", 0);
    fstWriterCreateVar(c, FST_VT_VCD_WIRE, FST_VD_IMPLICIT, 8, "b8_alias", b8);
    fstWriterCreateVar(c, FST_VT_VCD_REAL, FST_VD_IMPLICIT, 8, "r_alias", r);
    d1 = fstWriterCreateVar(c, FST_VT_VCD_WIRE, FST_VD_IMPLICIT, 4, "d1", 0);
    d2 = fstWriterCreateVar(c, FST_VT_VCD_WIRE, FST_VD_IMPLICIT, 4, "d2", 0);
    d3 = fstWriterCreateVar(c, FST_VT_VCD_WIRE, FST_VD_IMPLICIT, 1, "d3", 0);
    n1 = fstWriterCreateVar(c, FST_VT_VCD_WIRE, FST_VD_IMPLICIT, 1, "n1", 0);
    never = fstWriterCreateVar(c, FST_VT_VCD_WIRE, FST_VD_IMPLICIT, 3, "never", 0);
    fstWriterSetScope(c, FST_ST_VCD_GENERATE, "gen[0]", NULL);
    fstWriterSetAttrBegin(c, FST_AT_PACK, FST_PT_PACKED, "", 0);
    gv = fstWriterCreateVar(c, FST_VT_SV_INT, FST_VD_IMPLICIT, 32, "gv", 0);
    fstWriterSetAttrEnd(c);
    fstWriterSetUpscope(c);
    fstWriterSetScope(c, FST_ST_VCD_BEGIN, "", NULL);
    fstWriterCreateVar(c, FST_VT_VCD_WIRE, FST_VD_IMPLICIT, 1, "unnamed_clk", clk);
    fstWriterSetUpscope(c);
    fstWriterSetScope(c, FST_ST_VCD_BEGIN, NULL, NULL);
    fstWriterSetUpscope(c);
    fstWriterSetAttrBegin(c, FST_AT_MISC, FST_MT_UNKNOWN, "misc", 99);
    fstWriterSetAttrBegin(c, FST_AT_MISC, FST_MT_SUPVAR, "sup", 3);
    fstWriterSetScope(c, FST_ST_VHDL_ARCHITECTURE, "arch", "entity");
    fstWriterCreateVar(c, FST_VT_SV_BIT, FST_VD_IMPLICIT, 1, "ast", st);
    fstWriterSetUpscope(c);
    fstWriterSetScope(c, FST_ST_SV_ARRAY, "sva", NULL);
    fstWriterSetUpscope(c);
    fstWriterSetUpscope(c);

    fstWriterEmitValueChange(c, par, "1010");
    for (int t = 0; t <= 400; t++) {
        fstWriterEmitTimeChange(c, (uint64_t)t * 10);
        fstWriterEmitValueChange(c, clk, (t & 1) ? "1" : "0");
        if (t % 3 == 0) {
            buf[0] = states[(t / 3) % 16];
            buf[1] = 0;
            fstWriterEmitValueChange(c, st, buf);
        }
        if (t % 2 == 0) {
            bits(buf, 8, t % 10 ? "01" : "01xz");
            fstWriterEmitValueChange(c, b8, buf);
        }
        if (t % 5 == 0) {
            bits(buf, 13, "01");
            fstWriterEmitValueChange(c, b13, buf);
            bits(buf, 130, t % 20 ? "01" : "01xzuwlh-");
            fstWriterEmitValueChange(c, wide, buf);
            real(c, r, specials[(t / 5) % 9]);
            real(c, sr, (double)t / 7.0);
        }
        if (t % 7 == 0)
            real(c, rt, t * 1e-9);
        if (t % 4 == 1) {
            bits(buf, 32, "01");
            fstWriterEmitValueChange(c, in, buf);
            fstWriterEmitValueChange(c, gv, buf);
        }
        if (t % 50 == 3)
            fstWriterEmitValueChange(c, ev, "1");
        if (t % 9 == 0)
            fstWriterEmitVariableLengthValueChange(c, s, text, (uint32_t)(t % sizeof text));
        if (t % 6 == 0) {
            bits(buf, 14, "UDNZduxz01");
            fstWriterEmitValueChange(c, port, buf);
        }
        /* d1 and d2 change together before the flush, d3 and n1 after it only. */
        if (t % 4 == 0 && (t < 200 || t % 8 == 0)) {
            bits(buf, 4, "01");
            fstWriterEmitValueChange(c, d1, buf);
            if (t < 200)
                fstWriterEmitValueChange(c, d2, buf);
        }
        if (t >= 200 && t % 3 == 0) {
            fstWriterEmitValueChange(c, d3, (t & 2) ? "1" : "0");
            fstWriterEmitValueChange(c, n1, (t & 2) ? "1" : "0");
        }
        if (t == 150)
            fstWriterEmitDumpActive(c, 0);
        if (t == 180)
            fstWriterEmitDumpActive(c, 1);
        if (t == 200)
            fstWriterFlushContext(c);
    }
    (void)never;
}

/* Many small blocks, one of which starts at the time the previous one ended. */
static void multiblock(fstWriterContext *c)
{
    char buf[40];
    fstHandle a, v, w;

    fstWriterSetScope(c, FST_ST_VCD_MODULE, "mb", NULL);
    a = fstWriterCreateVar(c, FST_VT_VCD_WIRE, FST_VD_IMPLICIT, 1, "a", 0);
    v = fstWriterCreateVar(c, FST_VT_VCD_REG, FST_VD_IMPLICIT, 33, "v", 0);
    w = fstWriterCreateVar(c, FST_VT_VCD_WIRE, FST_VD_IMPLICIT, 1, "w", 0);
    fstWriterSetUpscope(c);
    for (int t = 0; t < 2000; t++) {
        fstWriterEmitTimeChange(c, 100 + (uint64_t)t * 3);
        fstWriterEmitValueChange(c, a, (rnd() & 1) ? "1" : "0");
        if (t % 11 == 0) {
            bits(buf, 33, "01x");
            fstWriterEmitValueChange(c, v, buf);
        }
        if (t % 97 == 0) {
            fstWriterFlushContext(c);
            fstWriterEmitTimeChange(c, 100 + (uint64_t)t * 3);
            fstWriterEmitValueChange(c, w, (t & 1) ? "1" : "0");
        }
    }
}

/* Large enough that the LZ4 writer packs the hierarchy twice (over 4 MiB). */
static void big_hier(fstWriterContext *c)
{
    enum { N = 48000 };
    static fstHandle h[N];
    char name[128];

    fstWriterSetScope(c, FST_ST_VCD_MODULE, "big", NULL);
    for (int i = 0; i < N; i++) {
        snprintf(name, sizeof name, "signal_%06d_%s", i,
                 "padding_padding_padding_padding_padding_padding_padding_padding_padding_pad");
        h[i] = fstWriterCreateVar(c, FST_VT_VCD_WIRE, FST_VD_IMPLICIT, 1, name, 0);
    }
    fstWriterSetUpscope(c);
    for (int t = 0; t < 20; t++) {
        fstWriterEmitTimeChange(c, (uint64_t)t);
        for (int i = t; i < N; i += 997)
            fstWriterEmitValueChange(c, h[i], ((i + t) & 1) ? "1" : "0");
    }
}

/* Initial values before the first time change, and a first change long after the start. */
static void late_start(fstWriterContext *c)
{
    fstHandle a, b, r;

    fstWriterSetScope(c, FST_ST_VCD_MODULE, "late", NULL);
    a = fstWriterCreateVar(c, FST_VT_VCD_WIRE, FST_VD_IMPLICIT, 1, "a", 0);
    b = fstWriterCreateVar(c, FST_VT_VCD_WIRE, FST_VD_IMPLICIT, 6, "b", 0);
    r = fstWriterCreateVar(c, FST_VT_VCD_REAL, FST_VD_IMPLICIT, 8, "r", 0);
    fstWriterSetUpscope(c);
    fstWriterEmitValueChange(c, a, "1");
    fstWriterEmitValueChange(c, b, "10z1x0");
    real(c, r, 3.25);
    fstWriterEmitTimeChange(c, 1000);
    fstWriterEmitValueChange(c, a, "0");
    fstWriterEmitTimeChange(c, 1005);
    fstWriterEmitValueChange(c, b, "111111");
    fstWriterEmitTimeChange(c, 2000);
    fstWriterEmitValueChange(c, a, "1");
}

/* Variables that never change but one, once. (libfst writes no hierarchy for a file without
   changes.) */
static void quiet(fstWriterContext *c)
{
    fstHandle a;

    fstWriterSetScope(c, FST_ST_VCD_TASK, "q", NULL);
    a = fstWriterCreateVar(c, FST_VT_VCD_WIRE, FST_VD_IMPLICIT, 1, "a", 0);
    fstWriterCreateVar(c, FST_VT_VCD_WIRE, FST_VD_IMPLICIT, 5, "b", 0);
    fstWriterSetUpscope(c);
    fstWriterEmitTimeChange(c, 0);
    fstWriterEmitTimeChange(c, 10);
    fstWriterEmitValueChange(c, a, "1");
}

static void tiny(fstWriterContext *c)
{
    fstHandle a;

    fstWriterSetScope(c, FST_ST_VCD_FUNCTION, "f", NULL);
    a = fstWriterCreateVar(c, FST_VT_VCD_WIRE, FST_VD_IMPLICIT, 1, "a", 0);
    fstWriterSetUpscope(c);
    for (int t = 0; t < 5; t++) {
        fstWriterEmitTimeChange(c, (uint64_t)t * 7);
        fstWriterEmitValueChange(c, a, (t & 1) ? "1" : "0");
    }
}

struct options
{
    enum fstWriterPackType pack;
    int repack, parallel, timescale;
    int64_t timezero;
};

static int write_case(const char *dir, const char *name, void (*body)(fstWriterContext *),
                      struct options o, int extensions)
{
    char fst[1024], vcd[1024];
    fstWriterContext *c;

    snprintf(fst, sizeof fst, "%s/%s_%s.fst", dir, VARIANT, name);
    c = fstWriterCreate(fst, 1);
    if (!c) {
        fprintf(stderr, "fst_ref: cannot create %s\n", fst);
        return 1;
    }
    fstWriterSetPackType(c, o.pack);
    fstWriterSetRepackOnClose(c, o.repack);
    fstWriterSetParallelMode(c, o.parallel);
    fstWriterSetTimescale(c, o.timescale);
    fstWriterSetTimezero(c, o.timezero);
    fstWriterSetDate(c, "Wed Oct 14 12:00:00 2026\n");
    fstWriterSetVersion(c, "fst_ref " VARIANT);
    seed = 1;
    body(c);
    fstWriterClose(c);

    snprintf(vcd, sizeof vcd, "%s/%s_%s.vcd", dir, VARIANT, name);
    if (to_vcd(fst, vcd, 0))
        return 1;
    if (extensions) {
        snprintf(vcd, sizeof vcd, "%s/%s_%s.ext.vcd", dir, VARIANT, name);
        if (to_vcd(fst, vcd, 1))
            return 1;
    }
    return 0;
}

static int corpus(const char *dir)
{
    static const struct
    {
        const char *suffix;
        enum fstWriterPackType pack;
    } packs[] = {{"z", FST_WR_PT_ZLIB}, {"f", FST_WR_PT_FASTLZ}, {"4", FST_WR_PT_LZ4}};
    static const int timescales[] = {2, 1, 0, -1, -2, -5, -7, -9, -10, -14, -16, -20, -21, 5};
    char name[64];
    int rc = 0;

    if (mkdir(dir, 0777) && errno != EEXIST) {
        perror(dir);
        return 1;
    }
    for (unsigned i = 0; i < sizeof packs / sizeof packs[0]; i++) {
        struct options o = {packs[i].pack, 0, 0, -9, 0};

        snprintf(name, sizeof name, "kitchen_%s", packs[i].suffix);
        rc |= write_case(dir, name, kitchen, o, 1);
        snprintf(name, sizeof name, "multiblock_%s", packs[i].suffix);
        rc |= write_case(dir, name, multiblock, o, 0);
        snprintf(name, sizeof name, "late_start_%s", packs[i].suffix);
        rc |= write_case(dir, name, late_start, o, 0);
        o.parallel = 1;
        snprintf(name, sizeof name, "parallel_%s", packs[i].suffix);
        rc |= write_case(dir, name, kitchen, o, 0);
    }
    {
        struct options o = {FST_WR_PT_ZLIB, 1, 0, -12, 0};
        rc |= write_case(dir, "repack", kitchen, o, 1);
        o.pack = FST_WR_PT_LZ4;
        rc |= write_case(dir, "repack_4", multiblock, o, 0);
    }
    {
        struct options o = {FST_WR_PT_ZLIB, 0, 0, -9, 0};
        rc |= write_case(dir, "quiet", quiet, o, 0);
        o.timezero = -42;
        rc |= write_case(dir, "timezero", tiny, o, 0);
    }
    if (!strcmp(VARIANT, "a2")) {
        struct options o = {FST_WR_PT_LZ4, 0, 0, -9, 0};
        rc |= write_case(dir, "big_hier_4", big_hier, o, 0);
        o.pack = FST_WR_PT_ZLIB;
        rc |= write_case(dir, "big_hier_z", big_hier, o, 0);
        for (unsigned i = 0; i < sizeof timescales / sizeof timescales[0]; i++) {
            o.timescale = timescales[i];
            snprintf(name, sizeof name, "timescale_%d", timescales[i]);
            rc |= write_case(dir, name, tiny, o, 0);
        }
    }
    return rc;
}

int main(int argc, char **argv)
{
    if (argc == 3 && !strcmp(argv[1], "corpus"))
        return corpus(argv[2]);
    if (argc == 4 && !strcmp(argv[1], "vcd"))
        return to_vcd(argv[2], argv[3], 0);
    if (argc == 5 && !strcmp(argv[1], "vcd") && !strcmp(argv[2], "-e"))
        return to_vcd(argv[3], argv[4], 1);
    fprintf(stderr, "usage: fst_ref corpus DIR | fst_ref vcd [-e] FST VCD\n");
    return 2;
}
