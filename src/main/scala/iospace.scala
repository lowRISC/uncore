// See LICENSE for license details.

package uncore
import Chisel._
import junctions._

abstract trait IOSpaceParameters extends UsesParameters {
  val xLen = params(XLen)
  val pALen = params(PAddrBits)
  require(xLen >= pALen) // TODO: support pALen > xLen
}

class IOSpaceConsts extends Module with IOSpaceParameters {
  val io = new Bundle {
    val base = Vec(UInt(OUTPUT, xLen), 2) // base for CSR
    val mask = Vec(UInt(OUTPUT, xLen), 2) // mask for CSR
    val paddr = UInt(INPUT, pALen)        // physical address for IO check
    val isIO = Bool(OUTPUT)               // indicate an IO address
  }

  val base = Wire(Vec(UInt(width = xLen), 2))
  val mask = Wire(Vec(UInt(width = xLen), 2))
  val check = Wire(Vec(Bool(), 2))

  base(0) := params(IOBaseAddr0)
  mask(0) := params(IOAddrMask0)
  base(1) := params(IOBaseAddr1)
  mask(1) := params(IOAddrMask1)

  io.base := base
  io.mask := mask

  for(i <- 0 until 2) {
    check(i) := (io.paddr & ~ mask(i)(pALen,0)) === base(i)(pALen,0)
  }
  io.isIO := check.reduce(_||_)
}

