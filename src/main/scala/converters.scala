package uncore

import Chisel._
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




}


/** Analyse the requests from MAM system interface.
  * If the burst length is larger than a chache line,
  * break the request and generate sub-requests to TileLink.
  */
class MamReqSerDes(implicit p: Parameters) extends TLModule()(p)
    with HasMamParameters
{
  val io = new Bundle {
    val mam = (new MamIOReqChannel).flip
    val tl = new MamIOReqChannel
    val tl_size = UInt(OUTPUT, mamBurstByteSizeWidth)
  }

  val cacheBlockBytes = UInt(p(CacheBlockBytes))

  val req = Reg(MamReq)
  val req_size = Reg(UInt(width = mamBurstByteSizeWidth))
  val req_valid = Reg(init=Bool(false))

  io.mam.ready := !req_valid
  io.tl.bits := req
  io.tl_size := req_size
  io.tl.valid := req_valid

  when(io.mam.fire()) {
    req_valid := Bool(true)
    req := io.mam.bits
    req_size := io.mam.bits.size * UInt(mamByteWidth)
  }

  when(io.tl.fire()) {
    when(req_size <= cacheBlockBytes) { // the last sub-request
      req_valid := Bool(false)
    }.elsewhen{
      req_size := req_size - cacheBlockBytes
    }
  }
}
