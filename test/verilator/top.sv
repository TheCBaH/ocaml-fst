// A small design traced with Verilator's --trace-fst: scalars, a counter, a wide register,
// packed structs, an array, a real and a parameter, across a few nested scopes.
module child #(parameter int W = 8) (input logic clk, input logic [W-1:0] d, output logic [W-1:0] q);
  always_ff @(posedge clk) q <= d ^ {W{q[0]}};
endmodule

module top (input logic clk);
  typedef struct packed { logic [3:0] tag; logic [11:0] payload; } item_t;
  localparam int DEPTH = 4;
  logic [7:0] count = 0;
  logic [127:0] wide = 128'h0123_4567_89ab_cdef_fedc_ba98_7654_3210;
  item_t items [DEPTH];
  logic [15:0] q16;
  real level = 0.0;
  integer cycles = 0;

  child #(.W(16)) u_child (.clk(clk), .d(items[count[1:0]]), .q(q16));

  always_ff @(posedge clk) begin
    count <= count + 1;
    wide <= {wide[0], wide[127:1]} ^ {120'b0, count};
    items[count[1:0]] <= {count[3:0], {4{count[2:0]}}};
    level <= level + 0.125 * $itor(count);
    cycles <= cycles + 1;
  end
endmodule
