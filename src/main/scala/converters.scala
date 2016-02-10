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

//  require(mamAddrWidth >= p(PAddrBits))
  require(mamAddrWidth >= params(PAddrBits))
  val buffer_width = max(tlDataBytes, mamByteWidth)
  //val buffer_size = p(CacheBlockBytes) + buffer_width
  val buffer_size = params(CacheBlockBytes) + buffer_width

  val reqSerDes = Module(new MamReqSerDes)
  reqSerDes.io.mam <> io.mam.req

  val data_buffer =
    Module(new SerDesBuffer(UInt(width=8), buffer_size, buffer_width, buffer_width))
  val mask_buffer =
    Module(new SerDesBuffer(Bool(), buffer_size, buffer_width, buffer_width))

  // assigning tl and mam signals
  io.tl.acquire.valid := Bool(false)
  io.tl.acquire.bits := Get(UInt(0), UInt(0), UInt(0))
  io.tl.grant.ready := !reqSerDes.io.tl_rw
  io.mam.wdata.ready := reqSerDes.io.tl_rw
  io.mam.rdata.valid := Bool(false)
  io.mam.rdata.bits := UInt(0)

  val tl_burst_beat_fire = reqSerDes.io.tl_block &&
    Mux(reqSerDes.io.tl_rw, io.tl.acquire.fire(), io.tl.grant.fire())

  val (tl_cnt, tl_finish) = Counter(tl_burst_beat_fire, tlDataBeats)

  when(reqSerDes.io.tl_valid && reqSerDes.io.tl_rw) { // write
    // MAM.wdata is connected to buffer inputs
    data_buffer.io.in.valid := io.mam.wdata.valid
    mask_buffer.io.in.valid := io.mam.wdata.valid
    data_buffer.io.in.bits := io.mam.wdata.bits.data
    data_buffer.io.in_size := UInt(buffer_width)
    mask_buffer.io.in.bits := Mux(reqSerDes.io.mam_burst,
                                  SInt(-1, width=buffer_width).toUInt,
                                  io.mam.wdata.bits.strb)
    mask_buffer.io.in_size := UInt(buffer_width)
    io.mam.wdata.ready := data_buffer.io.in.ready

    // tl.acquire is connected to buffer outputs
    when(tl_cnt === UInt(0) && data_buffer.io.count >= reqSerDes.io.tl_count ||
         tl_cnt =/= UInt(0) && data_buffer.io.count >= UInt(tlDataBeats)) {
      io.tl.acquire.valid := Bool(true)
      io.tl.acquire.bits :=
        Mux(reqSerDes.io.tl_block,
          PutBlock(UInt(0),
            reqSerDes.io.tl_addr >> (tlBeatAddrBits + tlByteAddrBits),
            tl_cnt << tlByteAddrBits,
            data_buffer.io.out.bits.toBits,
            mask_buffer.io.out.bits.toBits
          ),
          Put(UInt(0),
            reqSerDes.io.tl_addr >> (tlBeatAddrBits + tlByteAddrBits),
            reqSerDes.io.tl_addr(tlBeatAddrBits + tlByteAddrBits - 1, tlByteAddrBits),
            data_buffer.io.out.bits.toBits << (reqSerDes.io.tl_shift << UInt(3)),
            mask_buffer.io.out.bits.toBits << (reqSerDes.io.tl_shift << UInt(3))
          ))
    }
    data_buffer.io.out.ready := io.tl.acquire.ready
    mask_buffer.io.out.ready := io.tl.acquire.ready
    data_buffer.io.out_size :=
      Mux(reqSerDes.io.tl_block, UInt(tlDataBeats), reqSerDes.io.tl_count)
    mask_buffer.io.out_size :=
      Mux(reqSerDes.io.tl_block, UInt(tlDataBeats), reqSerDes.io.tl_count)
    
    reqSerDes.io.tl_ready :=
      Mux(reqSerDes.io.tl_block,
        io.tl.acquire.fire() && tl_finish,
        io.tl.acquire.fire())
  }

  when(reqSerDes.io.tl_valid && !reqSerDes.io.tl_rw) { // read
    // tl.grant is connected to buffer inputs
    data_buffer.io.in.valid := io.tl.grant.valid
    data_buffer.io.in.bits :=
      io.tl.grant.bits.data >> (reqSerDes.io.tl_shift << UInt(3))
    data_buffer.io.in_size := UInt(tlDataBytes) - reqSerDes.io.tl_shift
    io.tl.grant.ready := data_buffer.io.in.ready

    // MAM.rdata is connected to buffer outputs
    io.mam.rdata.valid := data_buffer.io.out.valid
    io.mam.rdata.bits := data_buffer.io.out.bits
    data_buffer.io.out.ready := io.mam.rdata.ready
    data_buffer.io.out_size := UInt(mamByteWidth)

    val tl_gnt_finished = Reg(init=Bool(false))
    when((!reqSerDes.io.tl_block || tl_finish) && io.tl.grant.fire()) {
      tl_gnt_finished := Bool(true)
    }

    when(data_buffer.io.count === UInt(mamByteWidth) && io.mam.rdata.fire()) {
      reqSerDes.io.tl_ready := Bool(true)
      tl_gnt_finished := Bool(false)
    }
  }
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
  val req_size = Reg(init=UInt(0,width = mamBurstByteSizeWidth))
  val req_valid = Reg(init=Bool(false))

  io.mam.req.ready := !req_valid

  when(io.mam.req.fire()) {
    req_valid := Bool(true)
    req := io.mam.req.bits
    req_size := io.mam.req.bits.size * UInt(mamByteWidth)
  }

  when(io.tl_valid && io.tl_ready) {
    req_size := req_size - io.tl_count
    when (req_size <= UInt(tlDataBytes) || req_size === UInt(cacheBytes)) {
      req_valid := Bool(false)
    }
  }

  io.tl_rw := req.rw
  io.tl_block := req.size >= UInt(cacheBytes) && req.addr(tlBlockAddrBits-1,0) === UInt(0)
  io.tl_addr := Cat(req.addr >> tlByteAddrBits, UInt(0, width=tlByteAddrBits))
  io.tl_shift := req.addr(tlByteAddrBits-1,0)
  io.tl_count := Mux(io.tl_block, UInt(cacheBytes), // a whole cache line
                 Mux(io.tl_shift === UInt(0), UInt(tlDataBytes), // a beat
                     UInt(tlDataBytes) - io.tl_shift))           // less than a beat
  io.tl_valid := req_size > UInt(0)
  io.mam_burst := req.burst
}
