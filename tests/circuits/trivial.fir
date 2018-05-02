circuit Trivial :
  module Trivial :
    input a: UInt<4>
    output out: SInt<5>
    input clk: Clock

    node i = not(a)

    wire fail: UInt<1>
    fail <= UInt(1)

    out <= mux(clk, i, SInt(-0x1))