package uncore

import Chisel._
import scala.math.max
import junctions._
import open_soc_debug._
import cde.Parameters

/** Bridge connect MAM system interface master to TileLink slave.
  * Provide a system interface master for read/write cache/memory
  */
class TileLinkIOMamIOConverter(implicit p: Parameters) extends TLModule()(p)
    with HasMamParameters
{
  val io = new Bundle {
    val mam = (new MemIO).flip
    val tl = new ClientUncachedTileLinkIO
  }

  require(mamAddrWidth >= p(PAddrBits))
  val Int buffer_width = max(tlDataBytes, mamByteWidth)
  val Int buffer_size = p(CacheBlockBytes) + buffer_width

  val reqSerDes = Module(new MamReqSerDes)
  reqSerDes.io.mam <> io.mam.req

  val data_buffer =
    Module(new SerDesBuffer(UInt(width=8), buffer_size, buffer_width, buffer_width))
  val mask_buffer =
    Module(new SerDesBuffer(Bool(), buffer_size, buffer_width, buffer_width))

  // assigning tl and mam signals
  io.tl.acquire.valid := Bool(false)
  io.tl.acquire.bits := Get(0, 0, 0)
  io.tl.grant.ready := !reqSerDes.io.tl_rw
  io.mam.wdata.ready := reqSerDes.io.tl_rw
  io.mam.rdata.valid := Bool(false)
  io.mam.rdata.bits := UInt(0)

  when(reqSerDes.io.tl_rw) { // write
    // MAM.wdata is connected to buffer inputs
    data_buffer.in.valid := io.mam.wdata.valid
    mask_buffer.in.valid := io.mam.wdata.valid
    io.mam.wdata.ready := data_buffer.in.ready
    data_buffer.in.bits := io.mam.wdata.bits.data
    mask_buffer.in.bits := Mux(reqSerDes.io.mam_burst,
                               SInt(-1, width=buffer_width).toUInt,
                               io.mam.wdata.bits.strb)

    // tl.acquire is connected to buffer outputs
  }.otherwise{ // read
    // tl.grant is connected to buffer inputs
    data_buffer.in.valid := io.tl.gnt.valid
    data_buffer.in.bits := io.tl.gnt.bits.data >> (reqSerDes.io.tl_shift << UInt(3))
    io.tl.gnt.ready := data_buffer.in.ready

    // MAM.rdata is connected to buffer outputs
  }


/** Analyse the requests from MAM system interface.
  * If the burst length is larger than a chache line,
  * break the request and generate sub-requests to TileLink.
  */
class MamReqSerDes(implicit p: Parameters) extends TLModule()(p)
    with HasMamParameters
{
  val cacheBytes = p(CacheBlockBytes)
  val io = new Bundle {
    val mam = (new MamIOReqChannel).flip
    val tl_rw = Bool(OUTPUT)                 // read/write request
    val tl_block = Bool(OUTPUT)              // beat or block
    val tl_addr = UInt(OUTPUT, width=p(PAddrBits)) // TileLink request address
    val tl_shift = UInt(OUTPUT, width=tlByteAddrBits) // shift data to align beat
    val tl_count = UInt(OUTPUT, width=tlBlockAddrBits+1) // number of data to read/write
    val mam_busrt = Bool(OUTPUT)             // whether to store strb
  }

  val req = Reg(MamReq)
  val req_size = Reg(UInt(width = mamBurstByteSizeWidth))
  val req_valid = Reg(init=Bool(false))

  io.mam.ready := !req_valid

  when(io.mam.fire()) {
    req_valid := Bool(true)
    req := io.mam.bits
    req_size := io.mam.bits.size * UInt(mamByteWidth)
  }

  when(io.tl.fire()) {
    when (req_size <= UInt(tlDataBytes) || req_size === UInt(cacheBlockBytes)) {
      req_valid := Bool(false)
    }.elsewhen{
      req_size := req_size - io.tl_count
    }
  }

  io.tl_rw := req.rw
  io.tl_block := req.size >= cacheBytes && req.addr(tlBlockAddrBits-1,0) === UInt(0)
  io.tl_addr := Cat(req.addr >> tlByteAddrBits, UInt(0, width=tlByteAddrBits))
  io.tl_shift := req.addr(tlByteAddrBits-1,0)
  io.tl_count := Mux(io.tl_block, cacheBytes, // a whole cache line
                 Mux(io.tl_shift === UInt(0), tlDataBytes, // a beat
                     tlDataBytes - io.tl_shift))           // less than a beat
  io.mam_burst := req.burst
}
