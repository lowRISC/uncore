// See LICENSE for license details.

package uncore
import Chisel._

abstract trait IOSpaceParameters extends UsesParameters {
  val xLen = params(XLen)
  val pALen = params(PAddrBits)
  val nIOSections = params(NIOSections)
  require(xLen >= pALen) // TODO: support pALen > xLen
}

class IOSpaceConsts extends Module with IOSpaceParameters {
  val io = new Bundle {
    val update = new ValidIO(new PCRUpdate).flip
    val paddr = UInt(INPUT, pALen)        // physical address for IO check
    val isIO = Bool(OUTPUT)               // indicate an IO address
  }

  // map registers
  // effecting pack
  val base = Reg(Vec(UInt(width=xLen), nIOSections))
  val mask = Reg(Vec(UInt(width=xLen), nIOSections))

  // update pack, enforced after a write to io_map_update
  val base_update = Reg(Vec(UInt(width=xLen), nIOSections))
  val mask_update = Reg(Vec(UInt(width=xLen), nIOSections))

  when(this.reset) {
    base_update(0) := UInt(params(InitIOBase))
    mask_update(0) := UInt(params(InitIOMask))

    // disable other IO sections
    if(NIOSections > 1) {
      for(i <- 1 until NIOSections) {
        base_update(i) := UInt(0)
        mask_update(i) := UInt(0)
      }
    }

    // copy update to effect
    for(i <- 0 until NIOSections) {
      base(i) := base_update(i)
      mask(i) := mask_update(i)
    }
  }

  // update logic
  for(i <- 0 until nIOSections) {
    when(io.update.valid && io.update.bits.addr === UInt(PCRs.pio_map + i*4 + 0)) {
      base_update(i) := io.update.bits.data
    }
    when(io.update.valid && io.update.bits.addr === UInt(PCRs.pio_map + i*4 + 1)) {
      mask_update(i) := io.update.bits.data
    }
  }
  when(io.update.valid && io.update.bits.addr === UInt(PCRs.pio_map_update)) {
    for(i <- 0 until NIOSections) {
      base(i) := base_update(i)
      mask(i) := mask_update(i)
    }
  }

  // checking logic
  val check = Vec(Bool(), nIOSections)
  for(i <- 0 until nIOSections) {
    check(i) := (io.paddr & ~ mask(i)(pALen,0)) === base(i)(pALen,0)
  }
  io.isIO := check.reduce(_||_)
}

