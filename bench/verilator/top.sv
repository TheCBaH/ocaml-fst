// Many lanes of registers, each updating at its own rate, for a large dense trace. MODE 0 is
// xorshift data in every lane, which hardly compresses; MODE 1 mixes it with counters and
// slowly changing registers, which do.
module top #(parameter int LANES = 512, parameter int MODE = 0) (input logic clk);
  logic [31:0] cyc = 0;
  real level = 0.0;

  always_ff @(posedge clk) begin
    cyc <= cyc + 1;
    level <= level + 0.125;
  end

  for (genvar i = 0; i < LANES; i++) begin : lane
    localparam int RATE = i % 6;
    logic [63:0] r = 64'(i + 1) * 64'h9E3779B97F4A7C15;
    logic [15:0] s = 0;
    logic [2:0] q = 0;
    logic b = 0;
    always_ff @(posedge clk)
      if ((cyc & ((32'd1 << RATE) - 1)) == 0) begin
        if (MODE == 0 || i % 4 == 3) r <= r ^ (r << 13) ^ (r >> 7) ^ (r << 17);
        else if (i % 4 == 0) r <= r + 1;
        else if (i % 4 == 1) r <= {r[62:0], ~r[63]};
        else if (cyc[7:0] == 0) r <= r + 64'h0101010101010101;
        s <= MODE == 0 ? s + r[15:0] : s + 1;
        q <= q + 1;
        b <= b ^ r[3];
      end
  end
endmodule
