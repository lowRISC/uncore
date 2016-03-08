// See LICENSE for license details.

package uncore
import Chisel._
import junctions._

abstract trait MemSpaceParameters extends UsesParameters {
  val xLen = params(XLen)
  val pALen = params(PAddrBits)
  val nMemSections = params(NMemSections)
  require(xLen >= pALen) // TODO: support pALen > xLen
}

class MemSpaceConsts(ch: Int) extends Module with MemSpaceParameters {
  val io = new Bundle {
    val update = new ValidIO(new PCRUpdate).flip
    val core_addr = Vec(UInt(INPUT, pALen), ch)  // address from core
    val phy_addr = Vec(UInt(OUTPUT, pALen), ch)  // physical address to outside
  }

  // map registers
  // effecting pack
  val cbase = Reg(Vec(UInt(width=xLen), nMemSections))
  val mask = Reg(Vec(UInt(width=xLen), nMemSections))
  val pbase = Reg(Vec(UInt(width=xLen), nMemSections))

  // update pack, enforced after a write to mem_map_update
  val cbase_update = Reg(Vec(UInt(width=xLen), nMemSections))
  val mask_update = Reg(Vec(UInt(width=xLen), nMemSections))
  val pbase_update = Reg(Vec(UInt(width=xLen), nMemSections))

  when(this.reset) {
    cbase_update(0) := UInt(params(InitMemBase))
    mask_update(0) := UInt(params(InitMemMask))
    pbase_update(0) := UInt(params(InitPhyBase))

    // disable other IO sections
    if(nMemSections > 1) {
      for(i <- 1 until nMemSections) {
        mask_update(i) := UInt(0)
      }
    }

    // copy update to effect
    for(i <- 0 until nMemSections) {
      cbase(i) := cbase_update(i)
      mask(i) := mask_update(i)
      pbase(i) := pbase_update(i)
    }
  }

  // update logic
  for(i <- 0 until nMemSections) {
    when(io.update.valid && io.update.bits.addr === UInt(PCRs.pmem_map + i*4 + 0)) {
      cbase_update(i) := io.update.bits.data
    }
    when(io.update.valid && io.update.bits.addr === UInt(PCRs.pmem_map + i*4 + 1)) {
      mask_update(i) := io.update.bits.data
    }
    when(io.update.valid && io.update.bits.addr === UInt(PCRs.pmem_map + i*4 + 2)) {
      pbase_update(i) := io.update.bits.data
    }
  }
  when(io.update.valid && io.update.bits.addr === UInt(PCRs.pmem_map_update)) {
    for(i <- 0 until nMemSections) {
      cbase(i) := cbase_update(i)
      mask(i) := mask_update(i)
      pbase(i) := pbase_update(i)
    }
  }

  // address converter
  for(c <- 0 until ch) {
    val addr_vec = Wire(Vec(UInt(width=pALen), nMemSections))
    for(i <- 0 until nMemSections) {
      addr_vec(i) :=
      Mux(mask(i) =/= UInt(0) &&
          (io.core_addr(c) & ~ mask(i)(pALen,0)) === cbase(i)(pALen,0),
          io.core_addr(c) & mask(i) | pbase(i),
          UInt(0)
      )
    }
    io.phy_addr(c) := addr_vec.reduce(_|_)
  }
}

