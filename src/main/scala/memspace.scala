// See LICENSE for license details.

package uncore
import Chisel._
import junctions._
import cde.{Parameters, Field}

abstract trait MemSpaceParameters{
  implicit val p: Parameters
  val xLen = p(XLen)
  val pALen = p(PAddrBits)
  val nMemSections = p(NMemSections)
  require(xLen >= pALen) // TODO: support pALen > xLen
}

class MemSpaceConsts(ch: Int)(implicit val p: Parameters) extends Module with MemSpaceParameters {
  val io = new Bundle {
    val update = new ValidIO(new PCRUpdate).flip
    val core_addr = Vec(ch, UInt(INPUT, pALen))  // address from core
    val phy_addr = Vec(ch, UInt(OUTPUT, pALen))  // physical address to outside
  }

  // map registers
  // effecting pack
  val cbase = Reg(Vec(nMemSections, UInt(width=xLen)))
  val mask = Reg(Vec(nMemSections, UInt(width=xLen)))
  val pbase = Reg(Vec(nMemSections, UInt(width=xLen)))

  // update pack, enforced after a write to mem_map_update
  val cbase_update = Reg(Vec(nMemSections, UInt(width=xLen)))
  val mask_update = Reg(Vec(nMemSections, UInt(width=xLen)))
  val pbase_update = Reg(Vec(nMemSections, UInt(width=xLen)))

  when(this.reset) {
    cbase_update(0) := UInt(p(InitMemBase))
    mask_update(0) := UInt(p(InitMemMask))
    pbase_update(0) := UInt(p(InitPhyBase))

    // disable other IO sections
    if(nMemSections > 1) {
      for(i <- 1 until nMemSections) {
        cbase_update(i) := UInt(0)
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
    val addr_vec = Wire(Vec(nMemSections, UInt(width=pALen)))
    for(i <- 0 until nMemSections) {
      addr_vec(i) :=
      Mux((io.core_addr(c) & ~ mask(i)(pALen,0)) === cbase(i)(pALen,0),
          io.core_addr(c) & mask(i) | pbase(i),
          UInt(0)
      )
    }
    io.phy_addr(c) := addr_vec.reduce(_|_)
  }
}

