// See LICENSE for license details.

// Tag controller for the LowRISC Rocket cores

package uncore
import Chisel._

//--------------------------------------------------------------//
// parameter definitions
//--------------------------------------------------------------//

case object TagBits extends Field[Int]
case object TagBaseAddr extends Field[Int]
case object TagMemSize extends Field[Int]
case object TagBlockBytes extends Field[Int]
case object TagRowBytes extends Field[Int]
case object TagCacheWays extends Field[Int]
case object TagCacheSets extends Field[Int]
case object TagCacheTrackers extends Field[Int]
case object TagRowBlocks extends Field[Int]
case object TagBlockBlocks extends Field[Int]
case object TagTLDataBits extends Field[Int]
case object TagBlockTagBits extends Field[Int]
case object TagRowTags extends Field[Int]
case object FPGAVer extends Field[Boolean]

abstract trait TagCacheParameters extends UsesParameters {
  
  // define for the tag
  val tagBits = params(TagBits)                   // the number of bits in each tag
  val coreDataBits = params(CoreDataBits)         // the number of bits in a data word (xpr)
  
  // the tag memory partition
  val tagMemSize = params(TagMemSize)             // the size of the tag memory partition is 2**TagMemSize bytes
  val tagBaseAddr = params(TagBaseAddr)           // the base address of the tag partition
  
  // the cache parameters
  val tagBlockBytes = params(TagBlockBytes)       // the cache block size in the tag cache
  val tagRowBytes = params(TagRowBytes)           // the size of a row in the tag cache
                                                  // always stores the tags for a L2 cache line
  val nWays = params(TagCacheWays)                // the number of ways in the tag cache
  val nSets = params(TagCacheSets)                // the number of sets in the tag cache

  // memory interface
  val mifDataBits = params(MIFDataBits)           // the width of a single memory read/write
  val mifDataBeats = params(MIFDataBeats)         // the number of packets in a memory IF burst

  // uncahched Tile link interface
  val tlDataBits = params(TLDataBits)             // the datawidth of a single tile linke packet 
  //val tlDataBeats = params(TLDataBeats)           // the number of packets in a tile link burst
  val tlDataBeats = 1                             // TileLink in the old Rocket-chip has only one beat 

  // structure parameters for the tag cache
  val nTrackers = params(TagCacheTrackers)        // the number of concurrent trackers
  val acqQueueDepth = tlDataBeats * 2             // the size of the Acquire queue
  val memQueueDepth = mifDataBeats * 2            // the size of the queue on memory interface

  // other parameters
  val refillCycles = tagBlockBytes / tagRowBytes  // the number of cycle required to refill a cache line
  val paddrBits = params(PAddrBits)               // size of a physical address
  val tagCacheIdxBits = log2Up(nSets)             // size of the index field
  val tagBlockRows = tagBlockBytes / tagRowBytes  // number of rows in a tag block
  val tagRowBlocks = params(TagRowBlocks)         // number of L2 blocks in a tag row
  val tagBlockTagBits = params(TagBlockTagBits)   // number of tag bits for a L2 block
  val blockOffBits = params(CacheBlockOffsetBits)
  val tagCacheUnRowAddrBits = log2Up(params(TagRowBlocks))
                                                  // the lower bits not used when access data array
  val tagCacheUnIndexBits = log2Up(params(TagBlockBlocks))
                                                  // the lower address not used for index
  val tagCahceUnTagBits = tagCacheIdxBits + tagCacheUnIndexBits
                                                  // the lower address not used for tag
  val tagCacheTagBits = paddrBits - tagCahceUnTagBits - blockOffBits
                                                  // the size of a tag
}


// deriving classes from parameters
abstract trait TagCacheModule extends Module with TagCacheParameters


//--------------------------------------------------------------//
// I/O definitions
//--------------------------------------------------------------//
trait TagCacheId extends Bundle with TagCacheParameters        { val id = UInt(width  = log2Up(nTrackers)) }
trait TagCacheMetadata extends Bundle with TagCacheParameters  { val tag = Bits(width = tagCacheTagBits + 2) }
trait TagCacheIdx extends Bundle with TagCacheParameters       { val idx = Bits(width = tagCacheIdxBits) }
trait TagCacheHit extends Bundle with TagCacheParameters       { val hit = Bool() }

class TagCacheMetaReadReq extends TagCacheId with TagCacheMetadata with TagCacheIdx

class TagCacheMetaWriteReq extends TagCacheMetadata with TagCacheIdx {
  val way_en = Bits(width = nWays)
}

class TagCacheMetaResp extends TagCacheId with TagCacheMetadata with TagCacheHit {
  val way_en = Bits(width = nWays)
}

class TagCacheMetaRWIO extends Bundle {
  val read = Decoupled(new TagCacheMetaReadReq)
  val write = Decoupled(new TagCacheMetaWriteReq)
  val resp = Valid(new TagCacheMetaResp).flip
}

trait TagCacheData extends Bundle with TagCacheParameters  { 
  val data = Bits(width = tagBlockTagBits * tagRowBlocks) 
}

trait TagCacheAddr extends Bundle with TagCacheParameters { 
  val addr = Bits(width = tagCacheIdxBits + log2Up(tagBlockRows)) 
}

class TagCacheDataReadReq extends TagCacheId with TagCacheAddr {
  val way_en = Bits(width = nWays)
}

class TagCacheDataWriteReq extends TagCacheData with TagCacheAddr {
  val way_en = Bits(width = nWays)
  val wmask = Bits(width = tagRowBlocks)
}

class TagCacheDataResp extends TagCacheId with TagCacheData

class TagCacheDataRWIO extends Bundle {
  val read = Decoupled(new TagCacheDataReadReq)
  val write = Decoupled(new TagCacheDataWriteReq)
  val resp = Valid(new TagCacheDataResp).flip
}

// combine memory cmd and data for single arbitration
class MemRequest extends MemReqCmd with HasMemData

//--------------------------------------------------------------//
// class definitions
//--------------------------------------------------------------//

// the top level of the tag cache
class TagCache extends TagCacheModule {

  // conditional IO to support performance counter
  class IOBundle extends Bundle {
    val uncached = new UncachedTileLinkIO().flip
    val mem = new MemPipeIO
  }

  class IOBundle_PFC extends IOBundle {
    val pfc = new CachePerformCounterReg
  }

  val io = new IOBundle_PFC

  // coherecne
  val co = params(TLCoherence)
  def gntHasData(m: LogicalNetworkIO[Grant]) = co.messageHasData(m.payload)
  def acqHasData(m: LogicalNetworkIO[Acquire]) = co.messageHasData(m.payload)

  // cache data arrays
  val meta = Module(new TagCacheMetadataArray)
  val data = Module(new TagCacheDataArray)
  //val victim = Module(new TagCacheVictimBuffer)

  // the trackers for outstanding memory requests
  val trackerList = (0 until nTrackers).map { id =>
    Module(new TagCacheTracker(id))
  }

  // buffer queues
  val acqQueue = Module(new Queue(io.uncached.acquire.bits.clone, acqQueueDepth))
  //val memRespQueue = Module(new Queue(new MemData, memQueueDepth))

  // arbiters
  def outputArbitration[T <: Data](out: DecoupledIO[T], ins: Seq[DecoupledIO[T]], count: Int = 1, lock: T => Bool = (a: T) => Bool(true)) {
    val arb = Module(new LockingRRArbiter(out.bits.clone, ins.size, count, lock))
    out <> arb.io.out
    arb.io.in zip ins map { case (a, in) => a <> in }
  }

  def inputRouting[T <: Data](in: ValidIO[T], outs: Seq[ValidIO[T]], tag: UInt) {
    outs.map(_.bits := in.bits)
    outs.zipWithIndex.map { case (o, i) => o.valid := in.valid && (UInt(i) === tag) }
  }

  // connections
  // uncached tileline
  // Acquire
  io.uncached.acquire <> acqQueue.io.enq
  val acq_arb = Module(new Arbiter(Bool(), trackerList.size))
  val acq_arb_idx = Vec(acq_arb.io.in.map(_.ready)).lastIndexWhere{b: Bool => b}
  val acq_match = Vec(trackerList.map(_.io.acq_match))
  val acq_match_idx = acq_match.lastIndexWhere{b: Bool => b}
  val acq_alloc_idx = Mux(acq_match.toBits.orR, acq_match_idx, acq_arb_idx)
  val acq_conflict = Vec(trackerList.map(_.io.acq_conflict)).toBits.orR
  acq_arb.io.in.zip(trackerList.map(_.io.uncached.acquire)).zipWithIndex.map { case((a, t), i) =>
    a.valid := t.ready
    t.bits := acqQueue.io.deq.bits
    t.valid := acqQueue.io.deq.valid && !acq_conflict && (acq_alloc_idx === UInt(i))
  }
  acq_arb.io.out.ready := acqQueue.io.deq.valid && !acq_conflict
  acqQueue.io.deq.ready := trackerList.map(_.io.uncached.acquire.ready).reduce(_||_) && !acq_conflict

  // Grant
  outputArbitration(io.uncached.grant, trackerList.map(_.io.uncached.grant), tlDataBeats, gntHasData _)
  
  // memory interface
  val memArbiter = Module(new MemArbiterInf(trackerList.size))
  memArbiter.io.req.zip(trackerList.map(_.io.mem_req)).map { case (a, r) => a <> r }
  io.mem.req_cmd <> memArbiter.io.mem_cmd
  io.mem.req_data <> memArbiter.io.mem_dat
  //inputRouting(io.mem.resp, trackerList.map(_.io.mem.resp), io.mem.resp.bits.tag)
  trackerList.map(_.io.mem_resp.bits := io.mem.resp.bits)
  trackerList.map(_.io.mem_resp).zipWithIndex.map { case (t, i) =>
    t.valid := io.mem.resp.valid && (UInt(i) === (io.mem.resp.bits.tag >> UInt(1,1))) // the lsb is used for tag/acq
  }

  
  // cache
  outputArbitration(meta.io.read, trackerList.map(_.io.meta.read))
  outputArbitration(meta.io.write, trackerList.map(_.io.meta.write))
  inputRouting(meta.io.resp, trackerList.map(_.io.meta.resp), meta.io.resp.bits.id)

  outputArbitration(data.io.read, trackerList.map(_.io.data.read))
  outputArbitration(data.io.write, trackerList.map(_.io.data.write))
  inputRouting(data.io.resp, trackerList.map(_.io.data.resp), data.io.resp.bits.id)

  //outputArbitration(victim.io.read, trackerList.map(_.io.victim.read))
  //outputArbitration(victim.io.write, trackerList.map(_.io.victim.write))
  //inputRouting(victim.io.resp, trackerList.map(_.io.victim.resp), victim.io.resp.bits.id)

  // cache performance counter
  if(params(UsePerformCounters)) {
    val cPC = Module(new CachePerformCounters)
    cPC.io.req.write := trackerList.map(_.io.pfc.write).reduce(_||_)
    cPC.io.req.write_miss := trackerList.map(_.io.pfc.write_miss).reduce(_||_)
    cPC.io.req.read := trackerList.map(_.io.pfc.read).reduce(_||_)
    cPC.io.req.read_miss := trackerList.map(_.io.pfc.read_miss).reduce(_||_)
    cPC.io.req.write_back := trackerList.map(_.io.pfc.write_back).reduce(_||_)
    io.pfc <> cPC.io.reg
  }
}

// an arbiter to put memory requests into CMD and DATA queues
class MemArbiterInf(n: Int) extends TagCacheModule {
  val io = new Bundle {
    val req = Vec.fill(n){Decoupled(new MemRequest).flip}
    val mem_cmd = Decoupled(new MemReqCmd)
    val mem_dat = Decoupled(new MemData)
  }

  def HasData(m: MemRequest) = m.rw

  val arb = Module(new LockingRRArbiter(io.req(0).bits.clone, n, mifDataBeats, HasData _))
  arb.io.in zip io.req map { case (a, in) => a <> in }

  val (dat_cnt, dat_done) = Counter(arb.io.out.fire() && arb.io.out.bits.rw, mifDataBeats)

  val memCMDQueue = Module(new Queue(new MemReqCmd, memQueueDepth))
  val memDataQueue = Module(new Queue(new MemData, memQueueDepth))

  memCMDQueue.io.enq.valid := arb.io.out.fire() && (dat_cnt === UInt(0))
  memCMDQueue.io.enq.bits := arb.io.out.bits

  memDataQueue.io.enq.valid := arb.io.out.fire() && arb.io.out.bits.rw
  memDataQueue.io.enq.bits := arb.io.out.bits

  when(dat_cnt === UInt(0)) {
    arb.io.out.ready := memCMDQueue.io.enq.ready && memDataQueue.io.enq.ready
  } .otherwise {
    arb.io.out.ready := memDataQueue.io.enq.ready
  }

  io.mem_cmd <> memCMDQueue.io.deq
  io.mem_dat <> memDataQueue.io.deq
}

// the request tracker
class TagCacheTracker(trackerId: Int) extends TagCacheModule {
  // conditional IO to support performance counter
  class IOBundle extends Bundle {
    val uncached = new UncachedTileLinkIO().flip
    val mem_req = Decoupled(new MemRequest)
    val mem_resp = Decoupled(new MemResp).flip
    val meta = new TagCacheMetaRWIO
    val data = new TagCacheDataRWIO
    val acq_conflict = Bool(OUTPUT)
    val acq_match = Bool(OUTPUT)
  }

  class IOBundle_PFC extends IOBundle {
    val pfc = new CachePerformCounterInput().flip
  }

  val io = new IOBundle_PFC

  // parameter requirements
  require(mifDataBits*mifDataBeats == tlDataBits*tlDataBeats)
  require(mifDataBits % (tagRowBytes * 8) == 0)
  require(tlDataBits % (tagRowBytes * 8) == 0)
  require(isPow2(nSets))
  require(isPow2(nWays))

  // states
  val s_idle :: s_meta_read :: s_meta_resp :: s_data_read_hit :: s_data_resp_hit :: s_data_write_hit :: s_data_read_wb :: s_data_resp_wb :: s_data_resp_wb_done :: s_write_back :: s_mem_req :: s_data_write_refill :: s_meta_write_refill :: s_meta_write_hit :: s_gnt :: s_busy :: Nil = Enum(UInt(), 16)
  val state = Reg(init=s_idle)

  // coherecne
  val co = params(TLCoherence)

  // tag utilities
  val tagUtil = new TagUtil(tagBits, coreDataBits)
  def tagIsValid(meta:Bits): Bool = meta(tagCacheTagBits+1)
  def tagIsDirty(meta:Bits): Bool = meta(tagCacheTagBits)
  def addrFromTag(tag: Bits, acq_addr: Bits): Bits = 
    Cat(tag(tag.getWidth - 2, 0), acq_addr(tagCahceUnTagBits-1, 0))
  def addrToTag(addr: Bits, dirty: Bool): Bits = Cat(UInt(1,1), dirty, UInt(addr) >> UInt(tagCahceUnTagBits))
  def tagAddrConv(addr:Bits): Bits = {
    // get the fill physical addr
    val full_addr = Cat(addr, Bits(0, blockOffBits))
    // shift to get tag addr
    val shifted_addr = full_addr >> UInt(tagCacheUnIndexBits)
    val tag_addr = Cat(Bits(tagBaseAddr,paddrBits)(paddrBits-1, tagMemSize),shifted_addr(tagMemSize-1,0))
    // remove lower block offsets
    tag_addr >> UInt(blockOffBits)
  }
  def addrToIndex(addr:Bits): Bits = addr(tagCahceUnTagBits-1, tagCacheUnIndexBits)
  def addrToRowAddr(addr:Bits): Bits = addr(tagCahceUnTagBits-1, tagCacheUnRowAddrBits)

  //----------------------signal definitions
  // acquire channel
  val c_acq = io.uncached.acquire
  val acq_src = Reg(c_acq.bits.payload.client_xact_id.clone)
  val acq_addr = Reg(c_acq.bits.payload.addr.clone)
  val acq_has_data = co.messageHasData(c_acq.bits.payload)
  val acq_wr = Reg(init=Bool(false))
  val acq_data = Vec.fill(tlDataBeats){Reg(c_acq.bits.payload.data.clone)}
  val (acq_data_cnt, acq_data_done) = Counter(c_acq.fire() && acq_has_data, tlDataBeats)
  val collect_acq_data = Reg(init=Bool(false))
  val acq_data_process = Reg(init=Bool(false)) // process the original data read/write requests
  val acq_data_no_tag = Vec((0 until tlDataBeats).map(i => tagUtil.removeTag(acq_data(i))))
  val acq_repl_meta = Reg(io.meta.resp.bits.tag.clone)
  //val acq_hit = Reg(init=Bool(false))
  val acq_way_en = Reg(init=Bits(0, nWays))

  // grant channel
  val c_gnt = io.uncached.grant
  val gnt_data = Vec.fill(tlDataBeats){c_gnt.bits.payload.data.clone}
  val (gnt_data_cnt, gnt_data_done) = Counter(c_gnt.fire() && !acq_wr, tlDataBeats)
  val gnt_tag = Reg(Bits(width=tagBlockTagBits))
  val gnt_enable = Reg(init=Bool(false))

  // memory interface
  val (mem_acq_data_write_cnt, mem_acq_data_write_done) =
    Counter(io.mem_req.fire() && acq_data_process && acq_wr, mifDataBeats)
  val mem_acq_data_write = Vec.fill(mifDataBeats){io.mem_req.bits.data.clone}
  val (mem_tag_data_write_cnt, mem_tag_data_write_done) =
    Counter(io.mem_req.fire() && state === s_write_back && !acq_data_process, mifDataBeats)
  val mem_tag_data_write = Vec.fill(mifDataBeats){io.mem_req.bits.data.clone}
  val c_mresp = io.mem_resp
  val mem_acq_data_read = Vec.fill(mifDataBeats){Reg(c_mresp.bits.data.clone)}
  val mem_tag_data_read = Vec.fill(mifDataBeats){Reg(c_mresp.bits.data.clone)}
  val mem_resp_is_acq = Bool()
  val mem_resp_is_tag = Bool()
  val (mem_acq_data_read_cnt, mem_acq_data_read_done) =
    Counter(c_mresp.valid && mem_resp_is_acq, mifDataBeats)
  val (mem_tag_data_read_cnt, mem_tag_data_read_done) =
    Counter(c_mresp.valid && mem_resp_is_tag, mifDataBeats)
  val mem_acq_gnt_ready = Reg(init=Bool(false))
  val mem_tag_refill_ready = Reg(init=Bool(false))

  // tag write back
  val mifTagRows = mifDataBits / (tagRowBytes * 8)
  val wb_data = Vec.fill(mifDataBeats*mifTagRows){Reg(io.data.resp.bits.data.clone)}
  val (wb_read_cnt, wb_read_done) = 
    Counter(io.data.read.fire() && state != s_data_read_hit, mifDataBeats*mifTagRows)
  val (wb_data_cnt, wb_data_done) = 
    Counter(io.data.resp.valid && state != s_data_resp_hit, mifDataBeats*mifTagRows)

  // tag refill
  val (refill_data_cnt, refill_data_done) =
    Counter(io.data.write.fire() && (state === s_data_write_refill), tagBlockBytes / tagRowBytes)
  val refill_data = Vec.fill(tagBlockBytes / tagRowBytes){io.data.write.bits.data.clone}

  //----------------------the uncached acquire channel
  // receiving the acquire with data
  c_acq.ready := Bool(false)
  when(collect_acq_data || state === s_idle) {
    c_acq.ready := Bool(true)
    when(c_acq.valid) {
      acq_data(acq_data_cnt) := c_acq.bits.payload.data
    }
    when(acq_data_done) {
      collect_acq_data := Bool(false)
    }
  }

  io.acq_conflict := addrToIndex(acq_addr) === addrToIndex(c_acq.bits.payload.addr) &&
                     (state != s_idle) && !collect_acq_data

  io.acq_match := acq_wr && (acq_addr === c_acq.bits.payload.addr) && collect_acq_data

  //----------------------the uncached grant channel
  gnt_data := gnt_data.fromBits(tagUtil.insertTag(mem_acq_data_read.toBits, gnt_tag))
  c_gnt.valid := Bool(false)
  //c_gnt.bits.payload := Grant(Bool(true), Grant.uncachedRead, 
  //  acq_src, UInt(0), gnt_data(gnt_data_cnt))
  c_gnt.bits.payload := Grant(UInt(0), acq_src, UInt(0), gnt_data(gnt_data_cnt))


  when(gnt_enable && mem_acq_gnt_ready) {
    c_gnt.valid := Bool(true)
    when(gnt_data_done) {
      gnt_enable := Bool(false)
      mem_acq_gnt_ready := Bool(false)
    }
  }

  //----------------------memory cmd and data requests interfaces
  // send out the acquire request to memory
  io.mem_req.valid := Bool(false)
  io.mem_req.bits.rw := acq_wr
  io.mem_req.bits.tag := Cat(UInt(trackerId), UInt(0,1)) // lsb == 0 denotes acq requests
  io.mem_req.bits.addr := acq_addr
  io.mem_req.bits.data := mem_acq_data_write(mem_acq_data_write_cnt)

  mem_acq_data_write := mem_acq_data_write.fromBits(acq_data_no_tag.toBits)
  when(acq_data_process && !collect_acq_data) {
    // wait until the whole acquire burst is received
    io.mem_req.valid := Bool(true)
    when(io.mem_req.ready && (!acq_wr || mem_acq_data_write_done)) {
      acq_data_process := Bool(false)
    }
  }

  // write back tags
  mem_tag_data_write := mem_tag_data_write.fromBits(wb_data.toBits)
  when(state === s_write_back && !acq_data_process) {
    // send out the command
    io.mem_req.bits.rw := Bool(true)
    io.mem_req.bits.tag := Cat(UInt(trackerId), UInt(1,1)) // lsb == 0 denotes tag requests
    io.mem_req.bits.addr := tagAddrConv(addrFromTag(acq_repl_meta, acq_addr))
    io.mem_req.valid := Bool(true)
    io.mem_req.bits.data := mem_tag_data_write(mem_tag_data_write_cnt)
  }

  // send tag missing request
  when(state === s_mem_req && !acq_data_process) {
    io.mem_req.bits.rw := Bool(false)
    io.mem_req.bits.tag := Cat(UInt(trackerId), UInt(1,1)) // lsb == 1 denotes tag requests
    io.mem_req.bits.addr := tagAddrConv(acq_addr)
    io.mem_req.valid := Bool(true)
  }

  //----------------------the memory response
  mem_resp_is_acq := io.mem_resp.bits.tag(UInt(0)) === UInt(0)
  mem_resp_is_tag := io.mem_resp.bits.tag(UInt(0)) === UInt(1)
  
  when(io.mem_resp.valid) {
    when(mem_resp_is_acq) {
      mem_acq_data_read(mem_acq_data_read_cnt) := io.mem_resp.bits.data
    }

    when(mem_resp_is_tag) {
      mem_tag_data_read(mem_tag_data_read_cnt) := io.mem_resp.bits.data
    }

    when(mem_tag_data_read_done) { mem_tag_refill_ready := Bool(true) }
    when(mem_acq_data_read_done) { mem_acq_gnt_ready := Bool(true) }
  }

  //----------------------meta interface
  io.meta.read.valid := Bool(false)
  io.meta.read.bits.id := UInt(trackerId)
  io.meta.read.bits.tag := addrToTag(acq_addr, Bool(false))
  io.meta.read.bits.idx := addrToIndex(acq_addr)
  
  when(state === s_meta_read) {
    io.meta.read.valid := Bool(true)
  }

  when(state === s_meta_resp && io.meta.resp.valid) {
    //acq_hit := io.meta.resp.bits.hit
    acq_repl_meta := io.meta.resp.bits.tag
    acq_way_en := io.meta.resp.bits.way_en
  }

  // meta write after refill
  io.meta.write.valid := Bool(false)
  io.meta.write.bits.tag := addrToTag(acq_addr, Bool(false))
  io.meta.write.bits.idx := addrToIndex(acq_addr)
  io.meta.write.bits.way_en := acq_way_en

  when(state === s_meta_write_refill) {
    io.meta.write.valid := Bool(true)
  }

  when(state === s_meta_write_hit) {
    io.meta.write.valid := Bool(true)
    io.meta.write.bits.tag := addrToTag(acq_addr, Bool(true))
  }

  //----------------------data array interface
  // read for hit
  io.data.read.valid := Bool(false)
  io.data.read.bits.id := UInt(trackerId)
  io.data.read.bits.addr := addrToRowAddr(acq_addr)
  io.data.read.bits.way_en := acq_way_en
  
  when(state === s_data_read_hit) {
    io.data.read.valid := Bool(true)
  }

  when(state === s_data_resp_hit) {
    val _rowOffset = acq_addr(tagCacheUnRowAddrBits-1, 0)
    gnt_tag := io.data.resp.bits.data >> (_rowOffset * UInt(tagBlockTagBits))
  }

  // read for write back
  when(state === s_data_read_wb || state === s_data_resp_wb) {
    io.data.read.valid := Bool(true)
    io.data.read.bits.addr := Cat(addrToIndex(acq_addr), wb_read_cnt)
  }

  when(state === s_data_resp_wb || state === s_data_resp_wb_done) {
    when(io.data.resp.valid) {
      wb_data(wb_data_cnt) := io.data.resp.bits.data
    }
  }

  // write for hit
  io.data.write.valid := Bool(false)
  io.data.write.bits.data := (
    UInt(tagUtil.extractTag(acq_data.toBits))
      << (acq_addr(tagCacheUnRowAddrBits-1, 0) * UInt(tagBlockTagBits))
  )
  io.data.write.bits.addr := addrToRowAddr(acq_addr)
  io.data.write.bits.wmask := UInt(1,1) << acq_addr(tagCacheUnRowAddrBits-1, 0)
  io.data.write.bits.way_en := acq_way_en

  when(state === s_data_write_hit && !collect_acq_data) {
    io.data.write.valid := Bool(true)
  }

  // write for refill
  refill_data := refill_data.fromBits(mem_tag_data_read.toBits)
  when(state === s_data_write_refill && mem_tag_refill_ready) {
    io.data.write.valid := Bool(true)
    io.data.write.bits.data := refill_data(refill_data_cnt)
    io.data.write.bits.addr := Cat(addrToIndex(acq_addr), refill_data_cnt)
    io.data.write.bits.wmask := SInt(-1)
  }

  //----------------------state machine
  switch (state) {
    is(s_idle) {
      when(io.uncached.acquire.valid) {
        acq_src := c_acq.bits.payload.client_xact_id
        acq_addr := c_acq.bits.payload.addr
        if(tlDataBeats > 1)
          collect_acq_data := acq_has_data
        acq_data_process := Bool(true)
        acq_wr := acq_has_data
        state := s_meta_read
      }
    }
    is(s_meta_read) {
      when(io.meta.read.ready) { state := s_meta_resp }
    }
    is(s_meta_resp) {
      when(io.meta.resp.valid) {
        state :=
        Mux(io.meta.resp.bits.hit,
          Mux(acq_wr, s_data_write_hit, s_data_read_hit),  // cache hit
          Mux(tagIsValid(io.meta.resp.bits.tag) && tagIsDirty(io.meta.resp.bits.tag),
            s_data_read_wb, // cache miss, WB needed
            s_mem_req))     // cache miss, WB not needed
      }
    }
    is(s_data_read_hit) {
      when(io.data.read.ready) { state := s_data_resp_hit }
    }
    is(s_data_resp_hit) {
      state := s_gnt
    }
    is(s_data_write_hit) {
      when(!collect_acq_data) { // ensure the acq messasge is received
        when(io.data.write.ready) { state := s_meta_write_hit }
      }
    }
    is(s_data_read_wb) {
      when(io.data.read.ready) { state := s_data_resp_wb }
    }
    is(s_data_resp_wb) {
      when(wb_read_done) { state := s_data_resp_wb_done }
    }
    is(s_data_resp_wb_done) {
      when(wb_data_done) { state := s_write_back }
    }
    is(s_write_back) {
      when(mem_tag_data_write_done) {
        state := s_mem_req
      }
    }
    is(s_mem_req) {
      when(!acq_data_process) { // ensure the original req sent
        when(io.mem_req.ready) { state := s_data_write_refill }
      }
    }
    is(s_data_write_refill) {
      when(refill_data_done) {
        mem_tag_refill_ready := Bool(false)
        state := s_meta_write_refill
      }
    }
    is(s_meta_write_refill) {
      when(io.meta.write.ready) {
        state := Mux(acq_wr, s_data_write_hit, s_data_read_hit)
      }
    }
    is(s_meta_write_hit) {
      when(io.meta.write.ready) {state := s_busy }
    }
    is(s_gnt) {
      gnt_enable := Bool(true)
      state := s_busy
    }
    is(s_busy) {
      when(!gnt_enable && !acq_data_process) { state := s_idle }
    }
  }

  // generate performance counter inputs
  if(params(UsePerformCounters)) {
    io.pfc.write := acq_wr && state === s_meta_read && io.meta.read.ready
    io.pfc.write_miss := acq_wr && state === s_meta_resp && !io.meta.resp.bits.hit
    io.pfc.read := !acq_wr && state === s_meta_read && io.meta.read.ready
    io.pfc.read_miss := !acq_wr && state === s_meta_resp && !io.meta.resp.bits.hit
    io.pfc.write_back := state === s_meta_resp && !io.meta.resp.bits.hit &&
                         tagIsValid(io.meta.resp.bits.tag) && tagIsDirty(io.meta.resp.bits.tag)
  }

}

// tag cache metadata array
class TagCacheMetadataArray extends TagCacheModule {
  val io = new TagCacheMetaRWIO().flip
  // the highest bit in the meta is the valid flag
  val meta_bits = tagCacheTagBits+2

  val metaArray = Mem(UInt(width = meta_bits*nWays), nSets, seqRead = true)
  val replacer = new RandomReplacement(nWays)

  // reset initial process
  val rst_cnt = Reg(init=UInt(0, log2Up(nSets+1)))
  val rst = rst_cnt < UInt(nSets)
  val rst_0 = rst_cnt === UInt(0)
  when (rst) { rst_cnt := rst_cnt+UInt(1) }

  // write request
  val waddr = Mux(rst, rst_cnt, io.write.bits.idx)
  val wdata = Mux(rst, UInt(0), io.write.bits.tag).toBits
  val wmask = Mux(rst, SInt(-1), io.write.bits.way_en)

  when (rst || (io.write.valid)) {
    metaArray.write(waddr, Fill(nWays, wdata), FillInterleaved(meta_bits, wmask))
  }

  // helpers
  def getTag(meta:Bits): Bits = meta(tagCacheTagBits-1,0)
  def isValid(meta:Bits): Bool = meta(tagCacheTagBits+1)
  def isDirty(meta:Bits): Bool = meta(tagCacheTagBits)

  // read from cache array
  val ctags = metaArray(RegEnable(io.read.bits.idx, io.read.valid))
  val ctagArray = Vec((0 until nWays).map(i => ctags((i+1)*meta_bits - 1, i*meta_bits)))

  // pipeline stage 1
  val s1_tag = RegEnable(io.read.bits.tag, io.read.valid)
  val s1_id = RegEnable(io.read.bits.id, io.read.valid)
  val s1_clk_en = Reg(next = io.read.fire())
  val s1_match_way = Vec((0 until nWays).map(i => (getTag(ctagArray(i)) === getTag(s1_tag) && isValid(ctagArray(i))))).toBits
  val s1_match_meta = ctagArray(OHToUInt(s1_match_way))
  val s1_hit = s1_match_way.orR()
  val s1_replace_way = UIntToOH(replacer.way)
  val s1_replace_meta = ctagArray(replacer.way)

  // pipeline stage 2
  val s2_match_way = RegEnable(s1_match_way, s1_clk_en)
  val s2_match_meta = RegEnable(s1_match_meta, s1_clk_en)
  val s2_hit = RegEnable(s1_hit, s1_clk_en)
  val s2_replace_way = RegEnable(s1_replace_way, s1_clk_en)
  val s2_replace_meta = RegEnable(s1_replace_meta, s1_clk_en)
  when(!io.resp.bits.hit && io.resp.valid) {replacer.miss}

  // response composition
  io.resp.valid := Reg(next = s1_clk_en)
  io.resp.bits.id := RegEnable(s1_id, s1_clk_en)
  io.resp.bits.hit := s2_hit
  io.resp.bits.way_en := Mux(s2_hit, s2_match_way, s2_replace_way)
  io.resp.bits.tag := Mux(s2_hit, s2_match_meta, s2_replace_meta)

  io.read.ready := !rst && !io.write.valid // so really this could be a 6T RAM
  io.write.ready := !rst
}

// tag cache data array
class TagCacheDataArray extends TagCacheModule {
  val io = new TagCacheDataRWIO().flip

  val waddr = io.write.bits.addr
  val raddr = io.read.bits.addr
  val wmask = FillInterleaved(tagBlockTagBits, io.write.bits.wmask)

  val resp = (0 until nWays).map { w =>
    val array = Mem(Bits(width=tagBlockTagBits*tagRowBlocks), nSets*refillCycles, seqRead = true)
    val reg_raddr = Reg(UInt())
    when (io.write.bits.way_en(w) && io.write.valid) {
      array.write(waddr, io.write.bits.data, wmask)
    }.elsewhen (io.read.bits.way_en(w) && io.read.valid) {
      reg_raddr := raddr
    }
    array(reg_raddr)
  }

  io.resp.valid := ShiftRegister(io.read.fire(), 1)
  io.resp.bits.id := ShiftRegister(io.read.bits.id, 1)
  io.resp.bits.data := Mux1H(ShiftRegister(io.read.bits.way_en, 1), resp)

  io.read.ready := !io.write.valid // TODO 1R/W vs 1R1W?
  io.write.ready := Bool(true)

}

// tag cache victim buffer
class TagCacheVictimBuffer extends TagCacheModule {
  val io = new TagCacheDataRWIO().flip

  val wmask = FillInterleaved(tagBlockTagBits, io.write.bits.wmask)
  // victim buffer is implemented in registers
  val array = Vec.fill(nWays){Reg(Bits(width=tagBlockTagBits * tagRowBlocks))}

  (0 until nWays).map { w =>
    when (io.write.bits.way_en(w) && io.write.valid) {
      array(w) := (array(w) & (~wmask)) | (io.write.bits.data & wmask)
    }
  }

  io.resp.valid := io.read.valid
  io.resp.bits.id := io.read.bits.id
  io.resp.bits.data := Mux1H(io.read.bits.way_en, array)
  io.read.ready := Bool(true)
  io.write.ready := Bool(true)
}
