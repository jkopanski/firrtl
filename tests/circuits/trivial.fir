n1circuit Trivial :
  module Trivial :
    input clock: Clock
    input reset: UInt<1>
    output b: UInt<4>

    b <= UInt<4>("b0010")
