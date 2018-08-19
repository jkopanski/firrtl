circuit Trivial :
  module Trivial :
    input clock: Clock
    input reset: UInt<1>
    output b: SInt<4>

    node n1 = SInt<4>(-4)
    node n2 = SInt<4>(3)
    b <= mux( UInt<1>(0), n1, n2)
