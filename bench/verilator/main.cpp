// Usage: Vtop OUT.fst CYCLES FLUSH_EVERY; ends a value change block every FLUSH_EVERY cycles.
#include <cstdlib>
#include <memory>

#include "Vtop.h"
#include "verilated.h"
#include "verilated_fst_c.h"

int main(int argc, char **argv)
{
    const char *out = argc > 1 ? argv[1] : "trace.fst";
    long cycles = argc > 2 ? atol(argv[2]) : 1000;
    long every = argc > 3 ? atol(argv[3]) : 0;
    auto ctx = std::make_unique<VerilatedContext>();
    ctx->traceEverOn(true);
    auto top = std::make_unique<Vtop>(ctx.get());
    VerilatedFstC tfp;
    top->trace(&tfp, 99);
    tfp.open(out);
    for (long c = 0; c < cycles; ++c) {
        for (int h = 0; h < 2; ++h) {
            top->clk = h;
            top->eval();
            tfp.dump(ctx->time());
            ctx->timeInc(5);
        }
        if (every && (c + 1) % every == 0)
            tfp.flush();
    }
    tfp.close();
    top->final();
    return 0;
}
