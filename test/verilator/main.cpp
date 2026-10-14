// Runs top for 2000 half cycles and writes the FST trace to argv[1].
#include <memory>

#include "Vtop.h"
#include "verilated.h"
#include "verilated_fst_c.h"

int main(int argc, char **argv)
{
    auto ctx = std::make_unique<VerilatedContext>();
    ctx->traceEverOn(true);
    auto top = std::make_unique<Vtop>(ctx.get());
    VerilatedFstC tfp;
    top->trace(&tfp, 99);
    tfp.open(argc > 1 ? argv[1] : "trace.fst");
    for (int i = 0; i < 2000; ++i) {
        top->clk = i & 1;
        top->eval();
        tfp.dump(ctx->time());
        ctx->timeInc(5);
    }
    tfp.close();
    top->final();
    return 0;
}
