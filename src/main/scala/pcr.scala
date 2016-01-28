// See LICENSE for license details.

package uncore

import Chisel._
import uncore._
import uncore.constants._
import cde.{Parameters, Field}

case object XLen extends Field[Int]
case object NIOSections extends Field[Int]
case object IODataBits extends Field[UInt]
case object NMemSections extends Field[Int]
case object InitIOBase extends Field[String]
case object InitIOMask extends Field[String]
case object InitMemBase extends Field[String]
case object InitMemMask extends Field[String]
case object InitPhyBase extends Field[String]

object CSR
{
  // commands
  val SZ = 3
  val X = BitPat.DC(SZ)
  val N = UInt(0,SZ)
  val W = UInt(1,SZ)
  val S = UInt(2,SZ)
  val C = UInt(3,SZ)
  val I = UInt(4,SZ)
  val R = UInt(5,SZ)

  val ADDRSZ = 12
}

object PCRs {
  val ptime     = 0x701
  val ptohost   = 0x780
  val pfromhost = 0x781
  val preset    = 0x782
  val pmem_map  = 0x7a0
  val pmem_map_update = 0x7af
  val pio_map   = 0x7b0
  val pio_map_update = 0x7bf
  val pint_map = 0x7c0
}


abstract trait PCRParameters {
  implicit val p: Parameters
  val xLen = p(XLen)
  val nCores = p(NTiles)
  val coreIdBits = log2Up(p(NTiles))
  val nMemSections = p(NMemSections)
  val nIOSections = p(NIOSections)
  val addrBits = 12
}

abstract class PCRBundle(implicit val p: Parameters) extends Bundle with PCRParameters
abstract class PCRModule(implicit val p: Parameters) extends Module with PCRParameters

class PCRReq(implicit p: Parameters) extends PCRBundle()(p) {
  val coreId = UInt(width = coreIdBits)
  val addr = UInt(width = addrBits)
  val cmd = UInt(width = CSR.SZ)
  val data = Bits(width = xLen)
}

class PCRResp(implicit p: Parameters) extends PCRBundle()(p) {
  val coreId = UInt(width = coreIdBits)
  val data = Bits(width = xLen)
}

class PCRUpdate(implicit p: Parameters) extends PCRBundle()(p) {
  val broadcast = Bool()
  val coreId = UInt(width = coreIdBits)
  val addr = UInt(width = addrBits)
  val data = Bits(width = xLen)
}

class PCRIO(implicit p: Parameters) extends PCRBundle()(p) {
  val req = new DecoupledIO(new PCRReq)
  val resp = new ValidIO(new PCRResp).flip
  val update = new ValidIO(new PCRUpdate).flip
}

class PCRControl(implicit p: Parameters) extends PCRModule()(p) {
  val io = new Bundle {
    val pcr_req = Vec(nCores, new DecoupledIO(new PCRReq)).flip
    val pcr_resp = new ValidIO(new PCRResp)
    val pcr_update = new ValidIO(new PCRUpdate)
    val soft_reset = Bool(OUTPUT)
    val host = new HIFIO
    val interrupt = UInt(INPUT, xLen)
    val irq = Vec(nCores, Bool(OUTPUT))
  }

  // global registers
  val reg_time = Reg(UInt(width=xLen))
  val reg_mem_map_core_base = Reg(Vec(nMemSections, UInt(width=xLen)))
  val reg_mem_map_mask = Reg(Vec(nMemSections, UInt(width=xLen)))
  val reg_mem_map_phy_base = Reg(Vec(nMemSections, UInt(width=xLen)))
  val reg_io_map_base = Reg(Vec(nIOSections, UInt(width=xLen)))
  val reg_io_map_mask = Reg(Vec(nIOSections, UInt(width=xLen)))
  val reg_tohost = Reg(init = UInt(0,xLen))
  val reg_tohost_coreId = Reg(UInt(width=coreIdBits))
  val reg_int_en = Reg(Vec(nCores, UInt(0,xLen)))
  val reg_int_pending = Reg(init = UInt(0,xLen))

  when(this.reset) {
    reg_mem_map_core_base(0) := UInt(p(InitMemBase))
    reg_mem_map_mask(0) := UInt(p(InitMemMask))
    reg_mem_map_phy_base(0) := UInt(p(InitPhyBase))
    reg_io_map_base(0) := UInt(p(InitIOBase))
    reg_io_map_mask(0) := UInt(p(InitIOMask))

    // disable other Memory sections
    if(nMemSections > 1) {
      for(i <- 1 until nMemSections) {
        reg_mem_map_mask(i) := UInt(0)
      }
    }

    // disable other IO sections
    if(nIOSections > 1) {
      for(i <- 1 until nIOSections) {
        reg_io_map_mask(i) := UInt(0)
      }
    }
  }

  // request arbitration
  val req_arb = Module(new RRArbiter(new PCRReq, nCores))
  req_arb.io.in <> io.pcr_req
  req_arb.io.out.ready :=
     reg_tohost === UInt(0) ||
     req_arb.io.out.bits.addr =/= UInt(PCRs.ptohost) && req_arb.io.out.valid

  // addr decoding
  val read_mapping = collection.mutable.LinkedHashMap[Int,Bits](
    PCRs.ptime -> reg_time,
    PCRs.ptohost -> UInt(0),
    PCRs.preset -> UInt(0)
  )

  // memory map
  for(i <- 0 until nMemSections) {
    read_mapping += PCRs.pmem_map + i*4 + 0 -> reg_mem_map_core_base(i)
    read_mapping += PCRs.pmem_map + i*4 + 1 -> reg_mem_map_mask(i)
    read_mapping += PCRs.pmem_map + i*4 + 2 -> reg_mem_map_phy_base(i)
  }
  read_mapping += PCRs.pmem_map_update -> UInt(0)

  // IO map
  for(i <- 0 until nIOSections) {
    read_mapping += PCRs.pio_map + i*4 + 0 -> reg_io_map_base(i)
    read_mapping += PCRs.pio_map + i*4 + 1 -> reg_io_map_mask(i)
  }
  read_mapping += PCRs.pio_map_update -> UInt(0)

  // interrupt map
  for(i <- 0 until nCores) {
    read_mapping += PCRs.pint_map + i*2 + 0 -> reg_int_en(i)
    read_mapping += PCRs.pint_map + i*2 + 1 -> (reg_int_en(i) & reg_int_pending)
  }

  val decoded_addr = read_mapping map { case (k, v) => k -> (req_arb.io.out.bits.addr === UInt(k)) }

  // read response
  io.pcr_resp.valid := req_arb.io.out.fire()
  io.pcr_resp.bits.coreId := req_arb.io.out.bits.coreId
  io.pcr_resp.bits.data := Mux1H(for ((k, v) <- read_mapping) yield decoded_addr(k) -> v)

  val wen = req_arb.io.out.fire() && req_arb.io.out.bits.cmd =/= CSR.R
  val wdata =
    Mux(req_arb.io.out.bits.cmd === CSR.C, io.pcr_resp.bits.data & ~req_arb.io.out.bits.data,
    Mux(req_arb.io.out.bits.cmd === CSR.S, io.pcr_resp.bits.data | req_arb.io.out.bits.data,
    req_arb.io.out.bits.data))

  // default update
  val update_arb = Module(new Arbiter(new PCRUpdate, 3))
  io.pcr_update.valid := update_arb.io.out.valid
  io.pcr_update.bits := update_arb.io.out.bits
  update_arb.io.out.ready := Bool(true)

  ///////////////////////////////////
  // Individual PCR registers

  // time
  // not writable, wall clock
  val wall_clock = Reg(init=UInt(0, xLen+6))
  wall_clock := wall_clock + UInt(1)
  reg_time := wall_clock >> 6; // should be replaced by a RTC
  update_arb.io.in(2).valid := wall_clock(5,0) === UInt(0)
  update_arb.io.in(2).bits.broadcast := Bool(true)
  update_arb.io.in(2).bits.coreId := UInt(0)
  update_arb.io.in(2).bits.addr := UInt(PCRs.ptime)
  update_arb.io.in(2).bits.data := reg_time

  // reset
  val soft_reset = Reg(init=Bool(false))
  val (soft_reset_cnt, soft_reset_done) = Counter(soft_reset, 20) // reset for 20 cycles
  io.soft_reset := soft_reset
  when(req_arb.io.out.fire() && decoded_addr(PCRs.preset)) {
    soft_reset := Bool(true)
  } .elsewhen(soft_reset_done) {
    soft_reset := Bool(false)
  }

  // to/from host
  when(wen && decoded_addr(PCRs.ptohost)) {
    reg_tohost := wdata;
    reg_tohost_coreId := req_arb.io.out.bits.coreId
  }

  io.host.req.valid := reg_tohost =/= UInt(0)
  io.host.req.bits.id := reg_tohost_coreId
  io.host.req.bits.data := reg_tohost

  when(io.host.req.fire()) {
    reg_tohost := UInt(0)
  }

  io.host.resp.ready := update_arb.io.in(1).ready
  update_arb.io.in(1).valid := io.host.resp.valid
  update_arb.io.in(1).bits.broadcast := Bool(false)
  update_arb.io.in(1).bits.coreId := io.host.resp.bits.id
  update_arb.io.in(1).bits.addr := UInt(PCRs.pfromhost)
  update_arb.io.in(1).bits.data := io.host.resp.bits.data

  // Other updates have no delay, normally broadcasted
  update_arb.io.in(0).valid := Bool(false)
  update_arb.io.in(0).bits.broadcast := Bool(true)
  update_arb.io.in(0).bits.coreId := UInt(0)
  update_arb.io.in(0).bits.addr := req_arb.io.out.bits.addr
  update_arb.io.in(0).bits.data := wdata

  // memory map
  for(i <- 0 until nMemSections) {
    when(wen && decoded_addr(PCRs.pmem_map + i*4 + 0)) {
      update_arb.io.in(0).valid := Bool(true)
      reg_mem_map_core_base(i) := wdata
    }

    when(wen && decoded_addr(PCRs.pmem_map + i*4 + 1)) {
      update_arb.io.in(0).valid := Bool(true)
      reg_mem_map_mask(i) := wdata
    }

    when(wen && decoded_addr(PCRs.pmem_map + i*4 + 2)) {
      update_arb.io.in(0).valid := Bool(true)
      reg_mem_map_phy_base(i) := wdata
    }
  }

  when(wen && decoded_addr(PCRs.pmem_map_update)) {
    update_arb.io.in(0).valid := Bool(true)
  }

  // IO map
  for(i <- 0 until nMemSections) {
    when(wen && decoded_addr(PCRs.pio_map + i*4 + 0)) {
      update_arb.io.in(0).valid := Bool(true)
      reg_io_map_base(i) := wdata
    }

    when(wen && decoded_addr(PCRs.pio_map + i*4 + 1)) {
      update_arb.io.in(0).valid := Bool(true)
      reg_io_map_mask(i) := wdata
    }
  }

  when(wen && decoded_addr(PCRs.pio_map_update)) {
    update_arb.io.in(0).valid := Bool(true)
  }

  // interrupt
  for(i <- 0 until nCores) {
    when(wen && decoded_addr(PCRs.pint_map +i*2 + 0)) {
      reg_int_en(i) := wdata
    }

    io.irq(i) := (reg_int_pending & reg_int_en(i)) =/= UInt(0)
  }

  val int_mask = reg_int_en.reduce(_|_)
  reg_int_pending := int_mask & io.interrupt;

}
