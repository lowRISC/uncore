// See LICENSE for license details.

package uncore
import Chisel._

abstract trait IOSpaceParameters extends UsesParameters {
  val xLen = params(XLen)
  val pALen = params(PAddrBits)
  require(xLen >= pALen) // TODO: support pALen > xLen
}

class IOSpaceConsts extends Moduel with IOSpaceParameters {
  val io = new Bundle {
    val base = Vec.fill(2){UInt(OUTPUT, xLen)} // base for CSR
    val mask = Vec.fill(2){UInt(OUTPUT, xLen)} // mask for CSR
    val paddr = UInt(INPUT, pALen)             // physical address for IO check
    val isIO = Bool(OUTPUT)                    // indicate an IO address 
  }

  val base = Vec.fill(2){UInt(width = xLen)}
  val mask = Vec.fill(2){UInt(width = xLen)}

  base(0) := params(IOBaseAddr0)
  mask(0) := params(IOAddrMask0)
  base(1) := params(IOBaseAddr1)
  mask(1) := params(IOAddrMask1)

  io.base := base
  io.mask := mask

  val check = (0 until 2).map(i =>
    io.paddr & ~ mask(i)(pALen,0) === base(i)(pALen,0)
  )

  io.isIO := check.map(_).reduce(_||_)
}

