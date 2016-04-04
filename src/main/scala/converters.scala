package uncore

import Chisel._
import scala.math.max
import junctions._
import open_soc_debug._
//import cde.Parameters

/** Bridge connect MAM system interface master to TileLink slave.
  * Provide a system interface master for read/write cache/memory
  */
//class TileLinkIOMamIOConverter(implicit p: Parameters) extends TLModule()(p)
class TileLinkIOMamIOConverter extends TLModule
    with HasMamParameters
{
  val io = new Bundle {
    val mam = (new MamIO).flip
    val tl = new ClientUncachedTileLinkIO
  }

  require(mamAddrBits >= params(PAddrBits))
  val cacheBlockBytes = params(CacheBlockBytes)

  val reqSerDes = Module(new MamReqSerDes)
  reqSerDes.io.mam.req <> io.mam.req
  reqSerDes.io.tl_ready := Bool(false)

  val mam_data_buffer =
    Module(new SerDesBuffer(UInt(width=8), cacheBlockBytes, mamBytes, tlDataBytes, UInt(0)))
  val mam_data_buffer_in_data = Wire(Vec(mamBytes, UInt(width=8)))
  mam_data_buffer.io.in.valid := io.mam.wdata.valid
  mam_data_buffer.io.out.ready := io.tl.acquire.ready
  mam_data_buffer.io.in_size := UInt(mamBytes)
  mam_data_buffer.io.out_size := reqSerDes.io.tl_count

  val mam_mask_buffer =
    Module(new SerDesBuffer(Bool(), cacheBlockBytes, mamBytes, tlDataBytes, Bool(false)))
  val mam_mask_buffer_in_data = Wire(Vec(mamBytes, Bool()))
  mam_mask_buffer.io.in.valid := io.mam.wdata.valid
  mam_mask_buffer.io.out.ready := io.tl.acquire.ready
  mam_mask_buffer.io.in_size := UInt(mamBytes)
  mam_mask_buffer.io.out_size := reqSerDes.io.tl_count

  val tl_data_buffer =
    Module(new SerDesBuffer(UInt(width=8), cacheBlockBytes, tlDataBytes, mamBytes, UInt(0)))
  val tl_data_buffer_in_data = Wire(Vec(tlDataBytes, UInt(width=8)))
  tl_data_buffer.io.in.valid := !reqSerDes.io.tl_rw && io.tl.grant.valid
  tl_data_buffer.io.out.ready := io.mam.rdata.ready
  tl_data_buffer.io.in_size := reqSerDes.io.tl_count
  tl_data_buffer.io.out_size := UInt(mamBytes)

  val tl_burst_beat_fire = reqSerDes.io.tl_block &&
    Mux(reqSerDes.io.tl_rw, io.tl.acquire.fire(), io.tl.grant.fire())

  val (tl_cnt, tl_finish) = Counter(tl_burst_beat_fire, tlDataBeats)

  val write_block =
    PutBlock(
      client_xact_id = UInt(0),
      addr_block = reqSerDes.io.tl_addr >> (tlBeatAddrBits + tlByteAddrBits),
      addr_beat = tl_cnt << tlByteAddrBits,
      data = mam_data_buffer.io.out.bits.toBits,
      wmask = mam_mask_buffer.io.out.bits.toBits
    )

  val beat_wmask = mam_mask_buffer.io.out.bits.toBits << reqSerDes.io.tl_shift
  val write_beat =
    Put(
      client_xact_id = UInt(0),
      addr_block = reqSerDes.io.tl_addr >> (tlBeatAddrBits + tlByteAddrBits),
      addr_beat = reqSerDes.io.tl_addr(tlBeatAddrBits + tlByteAddrBits - 1, tlByteAddrBits),
      data = mam_data_buffer.io.out.bits.toBits << (reqSerDes.io.tl_shift << UInt(3)),
      wmask = beat_wmask // mam_mask_buffer.io.out.bits.toBits << (reqSerDes.io.tl_shift << UInt(3))
    )

  val read_block =
    GetBlock(
      client_xact_id = UInt(0),
      addr_block = reqSerDes.io.tl_addr >> (tlBeatAddrBits + tlByteAddrBits)
    )

  val read_beat =
    Get(
      client_xact_id = UInt(0),
      addr_block = reqSerDes.io.tl_addr >> (tlBeatAddrBits + tlByteAddrBits),
      addr_beat = reqSerDes.io.tl_addr(tlBeatAddrBits + tlByteAddrBits - 1, tlByteAddrBits)
    )

  io.tl.acquire.valid := Bool(false)
  io.tl.acquire.bits :=
    Mux(reqSerDes.io.tl_rw,
      Mux(reqSerDes.io.tl_block, write_block, write_beat),
      Mux(reqSerDes.io.tl_block, read_block, read_beat))

  val tl_acq_sent = Reg(init=Bool(false))

  when(reqSerDes.io.tl_valid && reqSerDes.io.tl_rw) { // write
    // tl.acquire is connected to buffer outputs
    when(tl_cnt === UInt(0) && mam_data_buffer.io.count >= reqSerDes.io.tl_count ||
         tl_cnt =/= UInt(0) && mam_data_buffer.io.count >= UInt(tlDataBeats)) {
      io.tl.acquire.valid := !tl_acq_sent
    }
    
    when(io.tl.acquire.fire()) {
      tl_acq_sent := !reqSerDes.io.tl_block || tl_finish
    }

    reqSerDes.io.tl_ready := io.tl.grant.valid

    when(io.tl.grant.valid) {
      tl_acq_sent := Bool(false)
    }
  }
  mam_data_buffer.io.in.bits := mam_data_buffer_in_data.fromBits(io.mam.wdata.bits.data)
  mam_mask_buffer.io.in.bits := mam_mask_buffer_in_data.fromBits(
                                Mux(reqSerDes.io.mam_burst,
                                    SInt(-1, width=mamBytes).toUInt,
                                    io.mam.wdata.bits.strb))
  io.mam.wdata.ready := mam_data_buffer.io.in.ready

  when(reqSerDes.io.tl_valid && !reqSerDes.io.tl_rw) { // read
    // send out acquire
    when(!tl_acq_sent) {
      io.tl.acquire.valid := Bool(true)
      tl_acq_sent := io.tl.acquire.ready
    }

    reqSerDes.io.tl_ready :=
      Mux(reqSerDes.io.tl_block,
        io.tl.grant.fire() && tl_finish,
        io.tl.grant.fire())

    when(reqSerDes.io.tl_ready) {
      tl_acq_sent := Bool(false)
    }
  }
  tl_data_buffer.io.in.bits := tl_data_buffer_in_data.fromBits(
    io.tl.grant.bits.data >> (reqSerDes.io.tl_shift << UInt(3)))
  io.tl.grant.ready := Mux(reqSerDes.io.tl_rw, tl_acq_sent, tl_data_buffer.io.in.ready)
  io.mam.rdata.valid := tl_data_buffer.io.out.valid
  io.mam.rdata.bits.data := tl_data_buffer.io.out.bits.toBits

}


/** Analyse the requests from MAM system interface.
  * If the burst length is larger than a chache line,
  * break the request and generate sub-requests to TileLink.
  */
//class MamReqSerDes(implicit p: Parameters) extends TLModule()(p)
class MamReqSerDes extends TLModule
    with HasMamParameters
{
//  val cacheBytes = p(CacheBlockBytes)
  val cacheBytes = params(CacheBlockBytes)
  val io = new Bundle {
    val mam = (new MamIOReqChannel).flip
    val tl_rw = Bool(OUTPUT)                 // read/write request
    val tl_block = Bool(OUTPUT)              // beat or block
//    val tl_addr = UInt(OUTPUT, width=p(PAddrBits)) // TileLink request address
    val tl_addr = UInt(OUTPUT, width=params(PAddrBits)) // TileLink request address
    val tl_shift = UInt(OUTPUT, width=tlByteAddrBits) // shift data to align beat
    val tl_count = UInt(OUTPUT, width=tlBlockAddrBits+1) // number of data to read/write
    val tl_valid = Bool(OUTPUT)
    val tl_ready = Bool(INPUT)
    val mam_burst = Bool(OUTPUT)             // whether to store strb
  }

  val req = Reg(new MamReq)
  val byte_cnt = Reg(init=UInt(0,width = mamBytesBits))
  val req_valid = Reg(init=Bool(false))

  io.mam.req.ready := byte_cnt === UInt(0)

  when(io.mam.req.fire()) {
    req := io.mam.req.bits
    byte_cnt := io.mam.req.bits.beats * UInt(mamBytes)
  }

  when(io.tl_valid && io.tl_ready) {
    byte_cnt := byte_cnt - io.tl_count
    req.addr := req.addr + io.tl_count
  }

  io.tl_rw := req.rw
  io.tl_block := byte_cnt >= UInt(cacheBytes) && req.addr(tlBlockAddrBits-1,0) === UInt(0)
  io.tl_addr := Cat(req.addr >> tlByteAddrBits, UInt(0, width=tlByteAddrBits))
  io.tl_shift := req.addr(tlByteAddrBits-1,0)
  io.tl_count := Mux(io.tl_block, UInt(cacheBytes), // a whole cache line
                 Mux(byte_cnt +  io.tl_shift >= UInt(tlDataBytes), UInt(tlDataBytes) - io.tl_shift, // more than one beats
                     byte_cnt))      // less than one beat
  io.tl_valid := byte_cnt =/= UInt(0)
  io.mam_burst := req.burst
}
