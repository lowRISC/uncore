// See LICENSE for license details.

package uncore
import Chisel._
import junctions._
import cde.{Parameters, Field}

abstract trait IOSpaceParameters {
  implicit val p: Parameters
  val xLen = p(XLen)
  val pALen = p(PAddrBits)
  val nIOSections = p(NIOSections)
  require(xLen >= pALen) // TODO: support pALen > xLen
}

class IOSpaceConsts(ch: Int)(implicit val p: Parameters) extends Module with IOSpaceParameters {
  val io = new Bundle {
    val update = new ValidIO(new PCRUpdate).flip
    val paddr = Vec(ch, UInt(INPUT, pALen))  // physical address for IO check
    val isIO = Vec(ch, Bool(OUTPUT))         // indicate an IO address
  }

  // map registers
  // effecting pack
  val base = Reg(Vec(nIOSections, UInt(width=xLen)))
  val mask = Reg(Vec(nIOSections, UInt(width=xLen)))

  // update pack, enforced after a write to io_map_update
  val base_update = Reg(Vec(nIOSections, UInt(width=xLen)))
  val mask_update = Reg(Vec(nIOSections, UInt(width=xLen)))

  when(this.reset) {
    base_update(0) := UInt(p(InitIOBase))
    mask_update(0) := UInt(p(InitIOMask))

    // disable other IO sections
    if(nIOSections > 1) {
      for(i <- 1 until nIOSections) {
        base_update(i) := UInt(0)
        mask_update(i) := UInt(0)
      }
    }

    // copy update to effect
    for(i <- 0 until nIOSections) {
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
    for(i <- 0 until nIOSections) {
      base(i) := base_update(i)
      mask(i) := mask_update(i)
    }
  }

  // checking logic
  for(c <- 0 until ch) {
    val check = Wire(Vec(nIOSections, Bool()))
    for(i <- 0 until nIOSections) {
      check(i) := (io.paddr(c) & ~ mask(i)(pALen,0)) === base(i)(pALen,0)
    }
    io.isIO(c) := check.reduce(_||_)
  }
}

