// See LICENSE for license details.


// TODO: move this to bridge package


package uncore
import Chisel._
import scala.math.max

case object BusId extends Field[String]
case object NASTIDataBits extends Field[Int]
case object NASTIAddrBits extends Field[Int]
case object NASTIIdBits extends Field[Int]
case object NASTIUserBits extends Field[Int]

trait NASTIParameters extends UsesParameters {
  val nastiXDataBits = params(NASTIDataBits)
  val nastiWStrobeBits = nastiXDataBits / 8
  val nastiXOffBits = log2Up(nastiWStrobeBits)
  val nastiXAddrBits = params(NASTIAddrBits)
  val nastiXIdBits = params(NASTIIdBits)
  val nastiXUserBits = params(NASTIUserBits)
  val nastiXLenBits = 8
  val nastiXSizeBits = 3
  val nastiXBurstBits = 2
  val nastiXCacheBits = 4
  val nastiXProtBits = 3
  val nastiXQosBits = 4
  val nastiXRegionBits = 4
  val nastiXRespBits = 2

  def bytesToXSize(bytes: UInt) = MuxLookup(bytes, UInt("b111"), Array(
    UInt(1) -> UInt(0),
    UInt(2) -> UInt(1),
    UInt(4) -> UInt(2),
    UInt(8) -> UInt(3),
    UInt(16) -> UInt(4),
    UInt(32) -> UInt(5),
    UInt(64) -> UInt(6),
    UInt(128) -> UInt(7)))

  def opSizeToXSize(s: UInt) = MuxLookup(s, UInt("b111"), Array(
    MT_B  -> UInt(0),
    MT_H  -> UInt(1),
    MT_W  -> UInt(2),
    MT_D  -> UInt(3),
    MT_BU -> UInt(0),
    MT_HU -> UInt(1),
    MT_WU -> UInt(2)))
}

abstract class NASTIBundle extends Bundle with NASTIParameters
abstract class NASTIModule extends Module with NASTIParameters

trait NASTIChannel extends NASTIBundle
trait NASTIMasterToSlaveChannel extends NASTIChannel
trait NASTISlaveToMasterChannel extends NASTIChannel

//---------------------- fields ------------------------//
class NASTIMasterIO extends Bundle {
  val aw = Decoupled(new NASTIWriteAddressChannel)
  val w  = Decoupled(new NASTIWriteDataChannel)
  val b  = Decoupled(new NASTIWriteResponseChannel).flip
  val ar = Decoupled(new NASTIReadAddressChannel)
  val r  = Decoupled(new NASTIReadDataChannel).flip
}

class NASTILiteMasterIO extends Bundle {
  val aw = Decoupled(new NASTILiteWriteAddressChannel)
  val w  = Decoupled(new NASTILiteWriteDataChannel)
  val b  = Decoupled(new NASTILiteWriteResponseChannel).flip
  val ar = Decoupled(new NASTILiteReadAddressChannel)
  val r  = Decoupled(new NASTILiteReadDataChannel).flip
}

class NASTISlaveIO extends NASTIMasterIO { flip() }
class NASTILiteSlaveIO extends NASTILiteMasterIO { flip() }

// the detailed field definitions

trait HasNASTIId extends NASTIBundle {
  val id   = UInt(width = nastiXIdBits)
}

trait HasNASTIMetadata extends NASTIBundle {
  val len    = UInt(width = nastiXLenBits)
  val size   = UInt(width = nastiXSizeBits)
  val burst  = UInt(width = nastiXBurstBits)
  val lock   = Bool()
  val cache  = UInt(width = nastiXCacheBits)
}

trait HasNASTIData extends NASTIBundle {
  val data = UInt(width = nastiXDataBits)
}

trait HasWStrb extends NASTIBundle {
  val strb = UInt(width = nastiWStrobeBits)
}

trait HasNASTILast extends NASTIBundle {
  val last = Bool()
}

trait HasNASTIUser extends NASTIBundle {
  val user = UInt(width = nastiXUserBits)
}

//---------------------- channels ------------------------//
class NASTIAddressChannel extends NASTIMasterToSlaveChannel with HasNASTIId with HasNASTIUser {
  val addr   = UInt(width = nastiXAddrBits)
  val prot   = UInt(width = nastiXProtBits)
  val qos    = UInt(width = nastiXQosBits)
  val region = UInt(width = nastiXRegionBits)
}

class NASTIResponseChannel extends NASTISlaveToMasterChannel with HasNASTIId with HasNASTIUser {
  val resp = UInt(width = nastiXRespBits)
}

class NASTILiteWriteAddressChannel extends NASTIAddressChannel
class NASTIWriteAddressChannel extends NASTILiteWriteAddressChannel with HasNASTIMetadata

class NASTILiteReadAddressChannel extends NASTIAddressChannel
class NASTIReadAddressChannel extends NASTILiteReadAddressChannel with HasNASTIMetadata

class NASTILiteWriteDataChannel extends NASTIMasterToSlaveChannel with HasNASTIData with HasNASTIUser {
  val strb = UInt(width = nastiWStrobeBits)
}
class NASTIWriteDataChannel extends NASTILiteWriteDataChannel with HasNASTILast

class NASTILiteWriteResponseChannel extends NASTIResponseChannel
class NASTIWriteResponseChannel extends NASTILiteWriteResponseChannel

class NASTILiteReadDataChannel extends NASTIResponseChannel with HasNASTIData
class NASTIReadDataChannel extends NASTILiteReadDataChannel with HasNASTILast

//---------------------- Converters ------------------------//
class MemIONASTISlaveIOConverter extends MIFModule with NASTIParameters {
  val io = new Bundle {
    val nasti = new NASTISlaveIO
    val mem = new MemIO
  }

  require(mifDataBits == nastiXDataBits, "Data sizes between LLC and MC don't agree")
  val (mif_cnt_out, mif_wrap_out) = Counter(io.mem.resp.fire(), mifDataBeats)
  
  io.mem.req_cmd.bits.addr := Mux(io.nasti.aw.valid, io.nasti.aw.bits.addr, io.nasti.ar.bits.addr) >>
                                UInt(params(CacheBlockOffsetBits))
  io.mem.req_cmd.bits.tag := Mux(io.nasti.aw.valid, io.nasti.aw.bits.id, io.nasti.ar.bits.id)
  io.mem.req_cmd.bits.rw := io.nasti.aw.valid
  io.mem.req_cmd.valid := (io.nasti.aw.valid && io.nasti.b.ready) || io.nasti.ar.valid
  io.nasti.ar.ready := io.mem.req_cmd.ready && !io.nasti.aw.valid
  io.nasti.aw.ready := io.mem.req_cmd.ready && io.nasti.b.ready

  io.nasti.b.valid := io.nasti.aw.valid && io.mem.req_cmd.ready
  io.nasti.b.bits.id := io.nasti.aw.bits.id
  io.nasti.b.bits.resp := UInt(0)

  io.nasti.w.ready := io.mem.req_data.ready
  io.mem.req_data.valid := io.nasti.w.valid
  io.mem.req_data.bits.data := io.nasti.w.bits.data
  assert(!io.nasti.w.valid || io.nasti.w.bits.strb.andR, "MemIO must write full cache line")

  io.nasti.r.valid := io.mem.resp.valid
  io.nasti.r.bits.data := io.mem.resp.bits.data
  io.nasti.r.bits.last := mif_wrap_out
  io.nasti.r.bits.id := io.mem.resp.bits.tag
  io.nasti.r.bits.resp := UInt(0)
  io.mem.resp.ready := io.nasti.r.ready
}

class NASTIMasterIOTileLinkIOConverter extends TLModule with NASTIParameters {
  val io = new Bundle {
    val tl = new ManagerTileLinkIO
    val nasti = new NASTIMasterIO
  }

  val dataBits = tlDataBits*tlDataBeats 
  val dstIdBits = params(LNHeaderBits)
  require(tlDataBits == nastiXDataBits, "Data sizes between LLC and MC don't agree") // TODO: remove this restriction
  require(tlDataBeats < (1 << nastiXLenBits), "Can't have that many beats")
  require(dstIdBits + tlClientXactIdBits < nastiXIdBits, "NASTIMasterIO converter is going truncate tags: " + dstIdBits + " + " + tlClientXactIdBits + " >= " + nastiXIdBits)

  io.tl.acquire.ready := Bool(false)
  io.tl.probe.valid := Bool(false)
  io.tl.release.ready := Bool(false)
  io.tl.finish.ready := Bool(true)

  io.nasti.b.ready := Bool(false)
  io.nasti.r.ready := Bool(false)
  io.nasti.ar.valid := Bool(false)
  io.nasti.aw.valid := Bool(false)
  io.nasti.w.valid := Bool(false)

  val dst_off = dstIdBits + tlClientXactIdBits
  val acq_has_data = io.tl.acquire.bits.hasData()
  val rel_has_data = io.tl.release.bits.hasData()
  val is_write = io.tl.release.valid || (io.tl.acquire.valid && acq_has_data)

  // Decompose outgoing TL Acquires into NASTI address and data channels
  val active_out = Reg(init=Bool(false))
  val cmd_sent_out = Reg(init=Bool(false))
  val tag_out = Reg(UInt(width = nastiXIdBits))
  val addr_out = Reg(UInt(width = nastiXAddrBits))
  val has_data = Reg(init=Bool(false))
  val len_out = Reg(UInt(width = nastiXLenBits))
  val size_out = Reg(UInt(width = nastiXSizeBits))
  val data_from_rel = Reg(init=Bool(false))
  val (tl_cnt_out, tl_wrap_out) =
    Counter((io.tl.acquire.fire() && acq_has_data) ||
              (io.tl.release.fire() && rel_has_data), tlDataBeats)
  val tl_done_out = Reg(init=Bool(false))

  io.nasti.ar.bits.id := tag_out
  io.nasti.ar.bits.addr := addr_out
  io.nasti.ar.bits.len := len_out
  io.nasti.ar.bits.size := size_out
  io.nasti.ar.bits.burst := UInt("b01")
  io.nasti.ar.bits.lock := Bool(false)
  io.nasti.ar.bits.cache := UInt("b0000")
  io.nasti.ar.bits.prot := UInt("b000")
  io.nasti.ar.bits.qos := UInt("b0000")
  io.nasti.ar.bits.region := UInt("b0000")
  io.nasti.ar.bits.user := UInt(0)
  io.nasti.aw.bits := io.nasti.ar.bits
  io.nasti.w.bits.strb := Mux(data_from_rel, SInt(-1), io.tl.acquire.bits.wmask())
  io.nasti.w.bits.data := Mux(data_from_rel, io.tl.release.bits.data, io.tl.acquire.bits.data)
  io.nasti.w.bits.last := tl_wrap_out

  when(!active_out){
    io.tl.release.ready := io.nasti.w.ready
    io.tl.acquire.ready := io.nasti.w.ready && !io.tl.release.valid
    io.nasti.w.valid := (io.tl.release.valid && rel_has_data) ||
                        (io.tl.acquire.valid && acq_has_data)
    when(io.nasti.w.ready && (io.tl.release.valid || io.tl.acquire.valid)) {
      active_out := (!is_write && !io.nasti.ar.ready) ||
                    (is_write && !(io.nasti.aw.ready && io.nasti.w.ready)) ||
                    (io.nasti.w.valid && Bool(tlDataBeats > 1))
      io.nasti.aw.valid := is_write
      io.nasti.ar.valid := !is_write
      cmd_sent_out := (!is_write && io.nasti.ar.ready) || (is_write && io.nasti.aw.ready)
      tl_done_out := tl_wrap_out
      when(io.tl.release.valid) {
        data_from_rel := Bool(true)
        io.nasti.w.bits.data := io.tl.release.bits.data
        io.nasti.w.bits.strb := SInt(-1)
        val tag =  Cat(io.tl.release.bits.client_id,
                       io.tl.release.bits.client_xact_id,
                       io.tl.release.bits.isVoluntary())
        val addr = io.tl.release.bits.full_addr()
        io.nasti.aw.bits.id := tag
        tag_out := tag
        io.nasti.aw.bits.addr := addr
        addr_out := addr
        io.nasti.aw.bits.len := UInt(tlDataBeats-1)
        len_out := UInt(tlDataBeats-1)
        io.nasti.aw.bits.size := bytesToXSize(UInt(tlDataBytes))
        size_out := io.nasti.aw.bits.size
        has_data := rel_has_data
      } .elsewhen(io.tl.acquire.valid) {
        data_from_rel := Bool(false)
        io.nasti.w.bits.data := io.tl.acquire.bits.data
        io.nasti.w.bits.strb := io.tl.acquire.bits.wmask()
        val tag = Cat(io.tl.acquire.bits.client_id,
                      io.tl.acquire.bits.client_xact_id,
                      io.tl.acquire.bits.isBuiltInType())
        val addr = io.tl.acquire.bits.full_addr()
        when(is_write) {
          io.nasti.aw.bits.id := tag
          io.nasti.aw.bits.addr := addr
          io.nasti.aw.bits.len := Mux(io.tl.acquire.bits.isBuiltInType(Acquire.putBlockType),
                                    UInt(tlDataBeats-1), UInt(0)) 
          len_out := io.nasti.aw.bits.len
          io.nasti.aw.bits.size := bytesToXSize(PopCount(io.tl.acquire.bits.wmask()))
          size_out := io.nasti.aw.bits.size
        } .otherwise {
          io.nasti.ar.bits.id := tag
          io.nasti.ar.bits.addr := addr
          io.nasti.ar.bits.len := Mux(io.tl.acquire.bits.isBuiltInType(Acquire.getBlockType) || !io.tl.acquire.bits.isBuiltInType(),
                                    UInt(tlDataBeats-1), UInt(0)) 
          len_out := io.nasti.ar.bits.len
          io.nasti.ar.bits.size := Mux(io.tl.acquire.bits.isBuiltInType(), opSizeToXSize(io.tl.acquire.bits.op_size()), bytesToXSize(UInt(tlDataBytes)))
          size_out := io.nasti.ar.bits.size
        }
        tag_out := tag
        addr_out := addr
        has_data := acq_has_data
      }
    }
  }
  when(active_out) {
    io.nasti.ar.valid := !cmd_sent_out && !has_data
    io.nasti.aw.valid := !cmd_sent_out && has_data
    cmd_sent_out := cmd_sent_out || io.nasti.ar.fire() || io.nasti.aw.fire()
    when(has_data && !tl_done_out) {
      when(data_from_rel) {
        io.tl.release.ready := io.nasti.w.ready
        io.nasti.w.valid := io.tl.release.valid
      } .otherwise {
        io.tl.acquire.ready := io.nasti.w.ready
        io.nasti.w.valid := io.tl.acquire.valid
      }
    }
    when(tl_wrap_out) { tl_done_out := Bool(true) }
    when(cmd_sent_out && (!has_data || tl_done_out)) { active_out := Bool(false) }
  }

  // Aggregate incoming NASTI responses into TL Grants
  val (tl_cnt_in, tl_wrap_in) = Counter(io.tl.grant.fire() && io.tl.grant.bits.hasMultibeatData(), tlDataBeats)
  val gnt_arb = Module(new Arbiter(new GrantToDst, 2))
  io.tl.grant <> gnt_arb.io.out

  gnt_arb.io.in(0).valid := io.nasti.r.valid
  io.nasti.r.ready := gnt_arb.io.in(0).ready
  gnt_arb.io.in(0).bits := Grant(
    dst = (if(dstIdBits > 0) io.nasti.r.bits.id(dst_off, tlClientXactIdBits + 1) else UInt(0)),
    is_builtin_type = io.nasti.r.bits.id(0),
    g_type = Mux(io.nasti.r.bits.id(0), Grant.getDataBlockType, UInt(0)), // TODO: Assumes MI or MEI protocol
    client_xact_id = io.nasti.r.bits.id >> UInt(1),
    manager_xact_id = UInt(0),
    addr_beat = tl_cnt_in,
    data = io.nasti.r.bits.data)

  gnt_arb.io.in(1).valid := io.nasti.b.valid
  io.nasti.b.ready := gnt_arb.io.in(1).ready
  gnt_arb.io.in(1).bits := Grant(
    dst = (if(dstIdBits > 0) io.nasti.b.bits.id(dst_off, tlClientXactIdBits + 1) else UInt(0)),
    is_builtin_type = Bool(true),
    g_type = Mux(io.nasti.b.bits.id(0), Grant.voluntaryAckType, Grant.putAckType),
    client_xact_id = io.nasti.b.bits.id >> UInt(1),
    manager_xact_id = UInt(0))
}

class NASTILiteMasterIOTileLinkIOConverter extends TLModule with NASTIParameters with TileLinkParameters {
  val io = new Bundle {
    val tl = new ManagerTileLinkIO
    val nasti = new NASTILiteMasterIO
  }

  require(nastiXDataBits == 32)

  // request (transmit) states
  val t_idle :: t_req0 :: t_req1 :: t_busy :: Nil = Enum(UInt(), 4)
  val t_state = Reg(init=t_idle)

  // response (receiver) states
  val r_idle :: r_resp0 :: r_resp1 :: r_grant :: Nil = Enum(UInt(), 4)
  val r_state = Reg(init=r_idle)

  // internal transaction information
  val addr = io.tl.acquire.bits.full_addr()
  val client_id = RegEnable(io.tl.acquire.bits.client_id, io.tl.acquire.valid)
  val client_xact_id = RegEnable(io.tl.acquire.bits.client_xact_id, io.tl.acquire.valid)
  val grant_type = RegEnable(io.tl.acquire.bits.getBuiltInGrantType(), io.tl.acquire.valid)
  val op_size = RegEnable(io.tl.acquire.bits.op_size(), io.tl.acquire.valid)
  val data_buf = RegEnable(io.nasti.r.bits.data, r_state === r_resp0 && io.nasti.r.valid && op_size === MT_D) // the higher 32-bit for 64-bit read

  // set initial values for ports
  io.tl.probe.valid := Bool(false)
  io.tl.release.ready := Bool(false)

  io.nasti.aw.valid := Bool(false)
  io.nasti.w.valid := Bool(false)
  io.nasti.b.ready := Bool(false)
  io.nasti.ar.valid := Bool(false)
  io.nasti.r.ready := Bool(false)

  // drive IOs according to states

  // tl.Acquire
  io.tl.acquire.ready := t_state === t_busy && io.tl.acquire.valid // key addr and dara valid

  // tl.Grant
  io.tl.grant.valid := r_state === r_grant
  io.tl.grant.bits := Mux(grant_type === Grant.putAckType,
    Grant(client_id, Bool(true), grant_type, client_xact_id, UInt(0)),
    Grant(client_id, Bool(true), grant_type, client_xact_id, UInt(0), UInt(0),
          Cat(Mux(op_size === MT_D, data_buf, UInt(0)), io.nasti.r.bits.data)))

  // tl.Finish
  io.tl.finish.ready := Bool(true)

  // NASTI.AW
  val aw_fire = Reg(Bool())
  aw_fire := io.nasti.aw.fire()
  io.nasti.aw.valid := (t_state === t_req0 || t_state === t_req1) && grant_type === Grant.putAckType && ~aw_fire
  io.nasti.aw.bits.id := UInt(0)
  io.nasti.aw.bits.addr := Mux(op_size === MT_D && t_state === t_req0,
    addr | Cat(UInt(1), UInt(0, nastiXOffBits)), addr)
  io.nasti.aw.bits.prot := UInt("b000")
  io.nasti.aw.bits.region := UInt("b0000")
  io.nasti.aw.bits.qos := UInt("b0000")
  io.nasti.aw.bits.user := UInt(0)
  
  // NASTI.W
  val w_fire = Reg(Bool())
  w_fire := io.nasti.w.fire()
  io.nasti.w.valid := (t_state === t_req0 || t_state === t_req1) && grant_type === Grant.putAckType && ~w_fire

  val data_vec = Vec((0 until tlDataBits/nastiXDataBits).map(i =>
    io.tl.acquire.bits.data(i*nastiXDataBits + nastiXDataBits - 1, i*nastiXDataBits)))
  io.nasti.w.bits.data := data_vec(io.nasti.aw.bits.addr(tlByteAddrBits-1, nastiXOffBits))

  val mask_vec = Vec((0 until tlDataBits/nastiXDataBits).map(i =>
    io.tl.acquire.bits.wmask()(i*nastiWStrobeBits + nastiWStrobeBits - 1, i*nastiWStrobeBits)))
  io.nasti.w.bits.strb := mask_vec(io.nasti.aw.bits.addr(tlByteAddrBits-1, nastiXOffBits))

  // the write address and data combined fire
  val wr_fire = (io.nasti.aw.fire() || aw_fire) && (io.nasti.w.fire() || w_fire)

  // NASTI.AR
  io.nasti.ar.valid := (t_state === t_req0 || t_state === t_req1) && grant_type === Grant.getDataBeatType
  io.nasti.ar.bits := io.nasti.aw.bits

  // NASTI.B
  io.nasti.b.ready := (r_state === r_resp0 || r_state === r_resp1) && grant_type === Grant.putAckType

  // NASTI.R
  io.nasti.r.ready := (r_state === r_resp0 || r_state === r_resp1) && grant_type === Grant.getDataBeatType
  when(r_state === r_resp0 && grant_type === Grant.getDataBeatType && op_size === MT_D && io.nasti.r.valid) {
    data_buf := io.nasti.r.bits.data
  }

  // request state machine
  switch(t_state) {
    is(t_idle) {
      when(io.tl.acquire.valid) {
        t_state := t_req0
        aw_fire := Bool(false)
        w_fire := Bool(false)
      }
    }
    is(t_req0) {
      when(wr_fire || io.nasti.ar.fire()) {
        t_state := Mux(op_size === MT_D, t_req1, t_busy)
        aw_fire := Bool(false)
        w_fire := Bool(false)
      }
    }
    is(t_req1) {
      when(wr_fire || io.nasti.ar.fire()) {
        t_state := t_busy
        aw_fire := Bool(false)
        w_fire := Bool(false)
      }
    }
    is(t_busy) {
      when(io.tl.grant.fire()) {
        t_state := t_idle
      }
    }
  }

  // response state machine
  switch(r_state) {
    is(r_idle) {
      when(io.nasti.aw.fire() || io.nasti.ar.fire()) {
        r_state := r_resp0
      }
    }
    is(r_resp0) {
      when(io.nasti.b.fire() || io.nasti.r.fire()) {
        r_state := Mux(op_size === MT_D, r_resp1, r_grant)
      }
    }
    is(r_resp1) {
      when(io.nasti.b.fire() || io.nasti.r.fire()) {
        r_state := r_grant
      }
    }
    is(r_grant) {
      when(io.tl.grant.fire()) {
        r_state := r_idle
      }
    }
  }
}
