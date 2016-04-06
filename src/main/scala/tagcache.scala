// See LICENSE for license details.

package uncore
import Chisel._
import junctions._

import scala.collection.immutable.::
import scala.math._

//--------------------------------------------------------------//
// parameter definitions
//--------------------------------------------------------------//


abstract trait TagCacheParameters extends UsesParameters {

  // define for the tag
  val tagBits = params(TagBits)                   // the number of bits in each tag
  val coreDataBits = params(XLen)         // the number of bits in a data word (xpr)

  // the tag memory partition
  val tagMemSize = params(TagMemSize)             // the size of the tag memory partition is 2**TagMemSize bytes
  val tagMemSizeBram = params(TagMemSizeBram)             // the size of the tag memory partition is 2**TagMemSize bytes
  val tagBaseAddr = params(TCBaseAddr)           // the base address of the tag partition
  val tagBaseAddrBram = params(TCBaseAddrBram)           // the base address of the tag partition

  // the cache parameters
  val tagBlockBytes = params(TagBlockBytes)       // the cache block size in the tag cache
  val tagRowBytes = params(TagRowBytes)         // the size of a row in the tag cache
  // always stores the tags for a L2 cache line
  val nWays = params(NWays)                // the number of ways in the tag cache
  val nSets = params(NSets)                // the number of sets in the tag cache



  //  Tile link interface
  val tlDataBits = params(TLDataBits)             // the datawidth of a single tile linke packet
  val tlDataBeats = params(TLDataBeats)           // the number of packets in a tile link burst
  val tlDataBytes = tlDataBits/8
  val tlMaxClientXacts = params(TLMaxClientXacts)
  val tlMaxClientsPerPort = params(TLMaxClientsPerPort)
  val tlClientXactIdBits =  log2Up(tlMaxClientXacts*tlMaxClientsPerPort)
  val tlCoh = params(TLCoherencePolicy)
  val tlGrantTypeBits = max(log2Up(Grant.nBuiltInTypes),
    tlCoh.grantTypeWidth) + 1
  val tlBeatAddrBits = log2Up(tlDataBeats)
  val tlWriteMaskBits = params(TLWriteMaskBits)
  val tlByteAddrBits = log2Up(tlWriteMaskBits)

  // memory interface
  val mifDataBits = params(MIFDataBits)           // the width of a single memory read/write
  val mifDataBeats = tlDataBeats       // the number of packets in a memory IF burst

  // structure parameters for the tag cache
  val nTrackers = params(TCTrackers)        // the number of concurrent trackers
  //val acqQueueDepth = tlDataBeats * 2             // the size of the Acquire queue
  //val memQueueDepth = mifDataBeats * 2            // the size of the queue on memory interface

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

//----------------------------------------------------//
// I/O definitions
//--------------------------------------------------------------//
trait TagCacheId extends Bundle with TagCacheParameters        { val id = UInt(width  = log2Up(nTrackers)) }
trait TagCacheMetadata extends Bundle with TagCacheParameters  { val tag = Bits(width = tagCacheTagBits + 2 + 1) }
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

class TagCache extends TagCacheModule {
  val io = new Bundle {
    val tl = new ManagerTileLinkIO
    val update = new ValidIO(new PCRUpdate).flip
    val tl_out = new ClientUncachedTileLinkIO()
  }

  val conv = Module(new MemSpaceConsts(2))
  conv.io.update <> io.update

  conv.io.core_addr(0) := Mux(io.tl.acquire.bits.isBuiltInType(), io.tl.acquire.bits.full_addr(), io.tl.acquire.bits.addr_block << (tlBeatAddrBits + tlByteAddrBits))
  conv.io.core_addr(1) := io.tl.release.bits.addr_block << (tlBeatAddrBits + tlByteAddrBits)

  // coherecne
  //val co = params(TLCoherence)
  //def gntHasData(m: LogicalNetworkIO[Grant]) = co.messageHasData(m.payload)
  //def acqHasData(m: LogicalNetworkIO[Acquire]) = co.messageHasData(m.payload)

  // cache data arrays
  val meta = Module(new TagCacheMetadataArray)
  val data = Module(new TagCacheDataArray)
  //val victim = Module(new TagCacheVictimBuffer)


  io.tl.probe.valid := Bool(false)
  io.tl.release.ready := Bool(false)
  io.tl.finish.ready := Bool(true)

  // the trackers for outstanding memory requests
  val trackerList = (0 until nTrackers).map { id =>
    Module(new TagCacheTracker(id))
  }

  (0 until nTrackers) foreach { i =>
    trackerList(i).io.phy_addr_acq := conv.io.phy_addr(0)
    trackerList(i).io.phy_addr_rel := conv.io.phy_addr(1)
  }

  val tlAcqMatches = Vec(trackerList.map(_.io.tl_acq_match)).toBits
  val tlRelMatches = Vec(trackerList.map(_.io.tl_rel_match)).toBits
  val tlReady = Vec(trackerList.map(_.io.rdy)).toBits
  val tlAcqHandlerId = Mux(tlAcqMatches.orR,
    PriorityEncoder(tlAcqMatches),
    PriorityEncoder(tlReady))
  val tlRelHandlerId = Mux(tlRelMatches.orR,
    PriorityEncoder(tlRelMatches),
    PriorityEncoder(tlReady))
  val naBMatches = Vec(trackerList.map(_.io.na_b_match)).toBits
  val naRMatches = Vec(trackerList.map(_.io.na_r_match)).toBits
  val naBHandlerId = PriorityEncoder(naBMatches)
  val naRHandlerId = PriorityEncoder(naRMatches)

  def doInternalOutputArbitration[T <: Data](
                                              out: DecoupledIO[T],
                                              ins: Seq[DecoupledIO[T]],
                                              count: Int = 1,
                                              needsLock: Option[T => Bool] = None)
  {
    val arb = Module(new LockingRRArbiter(out.bits, ins.size, count, needsLock, true))
    out <> arb.io.out
    arb.io.in <> ins
  }

  def doInternalInputRouting[T <: Data](in: DecoupledIO[T], outs: Seq[DecoupledIO[T]], id: UInt) {
    outs.map(_.bits := in.bits)
    outs.zipWithIndex.map { case (o,i) => o.valid := in.valid && id === UInt(i) }
  }

  doInternalInputRouting(io.tl.acquire, trackerList.map(_.io.tl.acquire), tlAcqHandlerId)
  val acq_rdy = Vec(trackerList.map(_.io.tl.acquire.ready))
  io.tl.acquire.ready := (tlAcqMatches.orR || tlReady.orR) && acq_rdy(tlAcqHandlerId)

  doInternalInputRouting(io.tl.release, trackerList.map(_.io.tl.release), tlRelHandlerId)
  val rel_rdy = Vec(trackerList.map(_.io.tl.release.ready))
  io.tl.release.ready := (tlRelMatches.orR || tlReady.orR) && rel_rdy(tlRelHandlerId)

  doInternalOutputArbitration(io.tl.grant, trackerList.map(_.io.tl.grant))

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
  // cache
  outputArbitration(meta.io.read, trackerList.map(_.io.meta.read))
  outputArbitration(meta.io.write, trackerList.map(_.io.meta.write))
  inputRouting(meta.io.resp, trackerList.map(_.io.meta.resp), meta.io.resp.bits.id)

  outputArbitration(data.io.read, trackerList.map(_.io.data.read))
  outputArbitration(data.io.write, trackerList.map(_.io.data.write))
  inputRouting(data.io.resp, trackerList.map(_.io.data.resp), data.io.resp.bits.id)

  //UncachedTilelinkoutput
  val outerList = trackerList.map(_.io.tl_out)
  val outer_arb = Module(new ClientUncachedTileLinkIOArbiter(outerList.size))
  outer_arb.io.in <> outerList
  io.tl_out <> outer_arb.io.out

}

// the request tracker
class TagCacheTracker(id: Int) extends TagCacheModule with NASTIParameters{
  val io = new Bundle {
    val tl = new ManagerTileLinkIO()
    val rdy = Bool(OUTPUT)
    val meta = new TagCacheMetaRWIO
    val data = new TagCacheDataRWIO
    val tl_acq_match = Bool(OUTPUT)
    val tl_rel_match = Bool(OUTPUT)
    val na_b_match = Bool(OUTPUT)
    val na_r_match = Bool(OUTPUT)
    val phy_addr_acq = UInt(INPUT,width = nastiXAddrBits)
    val phy_addr_rel = UInt(INPUT,width = nastiXAddrBits)
    val tl_out = new ClientUncachedTileLinkIO()
  }

  // parameter requirements
  require(nastiXDataBits * tlDataBeats == tlDataBits * tlDataBeats)
  require(nastiXDataBits % (tagRowBytes * 8) == 0)
  require(tlDataBits % (tagRowBytes * 8) == 0)
  require(isPow2(nSets))
  require(isPow2(nWays))

  // states
  val s_idle :: s_dummy_wait :: s_meta_read :: s_meta_resp :: s_data_read_hit :: s_data_resp_hit :: s_data_write_hit :: s_data_read_wb :: s_data_resp_wb :: s_data_resp_wb_done :: s_write_back ::s_write_back_wait_b ::s_mem_req :: s_data_write_refill :: s_meta_write_refill :: s_meta_write_hit :: s_gnt :: s_busy :: Nil = Enum(UInt(), 18)
  val state = Reg(init=s_idle)

  val physical_dram_start = UInt("h40000000")

   // tag utilities
  val tagUtil = new TagUtil(tagBits, coreDataBits)
  def tagIsDram(meta:Bits): Bool = meta(tagCacheTagBits + 2)
  def tagIsValid(meta:Bits): Bool = meta(tagCacheTagBits+1)
  def tagIsDirty(meta:Bits): Bool = meta(tagCacheTagBits)
  def addrFromTag(tag: Bits, acq_addr: Bits): Bits =
    Cat(tag(tag.getWidth - 3, 0), acq_addr(tagCahceUnTagBits-1, 0))
  def addrToTag(addr: Bits, dirty: Bool, is_dram : Bool): Bits = Cat(is_dram, UInt(1,1), dirty, UInt(addr) >> UInt(tagCahceUnTagBits))
  def tagAddrConv(addr:Bits, is_dram : Bool): Bits = {
    // get the full physical addr
    val full_addr = Cat(addr, Bits(0, blockOffBits))
    // shift to get tag addr
    val shifted_addr = full_addr >> UInt(tagCacheUnIndexBits)
   // val tag_addr = Mux(is_dram_address,  Cat(Bits(tagBaseAddr,paddrBits)(paddrBits-1, tagMemSize),shifted_addr(tagMemSize-1,0)), UInt("hF000"))
   val tag_addr = Mux(is_dram,  Cat(Bits(tagBaseAddr,paddrBits)(paddrBits-1, tagMemSize),shifted_addr(tagMemSize-1,0)), Cat(Bits(tagBaseAddrBram,paddrBits)(paddrBits-1, tagMemSizeBram),shifted_addr(tagMemSizeBram-1,0)))

    // remove lower block offsets
    tag_addr >> UInt(blockOffBits)
  }
  def addrToIndex(addr:Bits): Bits = addr(tagCahceUnTagBits-1, tagCacheUnIndexBits)
  def addrToRowAddr(addr:Bits): Bits = addr(tagCahceUnTagBits-1, tagCacheUnRowAddrBits)


  private def opSizeToXSize(ops: UInt) = MuxLookup(ops, UInt("b111"), Seq(
    MT_B  -> UInt(0),
    MT_BU -> UInt(0),
    MT_H  -> UInt(1),
    MT_HU -> UInt(1),
    MT_W  -> UInt(2),
    MT_WU -> UInt(2),
    MT_D  -> UInt(3),
    MT_Q  -> UInt(log2Up(tlDataBytes))))

  // liminations:
  val dataBits = tlDataBits*tlDataBeats
  val dstIdBits = params(LNHeaderBits)
  require(tlDataBits == nastiXDataBits, "Data sizes between LLC and MC don't agree") // TODO: remove this restriction
  require(tlDataBeats < (1 << nastiXLenBits), "Can't have that many beats")
  require(dstIdBits + tlClientXactIdBits < nastiXIdBits, "NASTIIO converter is going truncate tags: " + dstIdBits + " + " + tlClientXactIdBits + " >= " + nastiXIdBits)
  // assume MI or MEI protocol


  // rename signals
  val tl_acq = io.tl.acquire.bits
  val tl_rel = io.tl.release.bits
  val tl_gnt = io.tl.grant.bits
  val tl_fin = io.tl.finish.bits

  val is_read = Reg(init=Bool(false))
  val is_write = Reg(init=Bool(false))
  val is_acq = Reg(init=Bool(false))
  val is_builtin = Reg(init=Bool(false))
  val tag_out = Reg(UInt(width = nastiXIdBits))
  val addr_out = Reg(UInt(width = nastiXAddrBits))
  val len_out = Reg(UInt(width = nastiXLenBits))
  val size_out = Reg(UInt(width = nastiXSizeBits))
  val g_type_out = Reg(UInt(width = tlGrantTypeBits))
  val cmd_sent = Reg(init=Bool(false))
  val is_idle = !(is_read || is_write)


  val acq_addr = addr_out >> UInt(blockOffBits)
  val is_dram_address = Mux(((addr_out.toBits & physical_dram_start.toBits) === physical_dram_start.toBits).toBool(), Bool(true), Bool(false))

  //Tag signals and definitions
  val collect_acq_data = Reg(init=Bool(false))
  val acq_data_process = Reg(init=Bool(false)) // process the original data read/write requests
  val acq_data = Reg(Vec.fill(tlDataBeats){io.tl.acquire.bits.data.clone})
  val (acq_data_cnt, acq_data_done) = Counter((io.tl.acquire.fire() || io.tl.release.fire()) && is_write, tlDataBeats)
  val acq_rel_input_data_without_tag = Mux(is_acq, tagUtil.removeTag(tl_acq.data) , tagUtil.removeTag(tl_rel.data))
  val acq_repl_meta = Reg(io.meta.resp.bits.tag.clone)
  val acq_way_en = Reg(init=Bits(0, nWays))
  val mem_acq_data_read = Reg(Vec.fill(tlDataBeats){UInt(width=tlDataBits)})
  val mem_tag_data_read = Reg(Vec.fill(tlDataBeats){UInt(width=tlDataBits)})

  val (mem_tag_data_write_cnt, mem_tag_data_write_done) =
    Counter(io.tl_out.acquire.fire() && state === s_write_back && !acq_data_process, tlDataBeats)
  val mem_tag_refill_ready = Reg(init=Bool(false))

  val is_read_precess = Reg(init=Bool(false))
  val is_write_process = Reg(init=Bool(false))

  val gnt_enable = Reg(init=Bool(false))
  val gnt_data = Reg(Vec.fill(tlDataBeats){io.tl.grant.bits.data.clone})
  val (gnt_data_cnt, gnt_data_done) = Counter(io.tl.grant.fire() && !is_write_process, tlDataBeats)

  val gnt_tag = Reg(Bits(width=tagBlockTagBits))

  val mifTagRows = mifDataBits / (tagRowBytes * 8)
  val wb_data = Reg(Vec.fill(mifDataBeats*mifTagRows){io.data.resp.bits.data.clone})
  val (wb_read_cnt, wb_read_done) =
    Counter(io.data.read.fire() && state != s_data_read_hit, tlDataBeats * mifTagRows)
  val (wb_data_cnt, wb_data_done) =
    Counter(io.data.resp.valid && state != s_data_resp_hit, tlDataBeats * mifTagRows)

  val mem_tag_data_write = Reg(Vec.fill(tlDataBeats){UInt(width=tlDataBits)})

  // tag refill
  val (refill_data_cnt, refill_data_done) =
    Counter(io.data.write.fire() && (state === s_data_write_refill), tagBlockBytes / tagRowBytes)
  val refill_data = Reg(Vec.fill(tagBlockBytes / tagRowBytes){io.data.write.bits.data.clone})

  val mem_send_tag = (state === s_write_back && !acq_data_process)
  val mem_receive_tag =  (state === s_mem_req && !acq_data_process)


  // Converter internal control signalspayload
  val write_multiple_data = Reg(init=Bool(false))
  val read_multiple_data = Reg(init=Bool(false))

  val (nw_cnt, nw_finish) =
    Counter(io.tl_out.acquire.fire() && ((write_multiple_data && is_write && acq_data_process) || mem_send_tag)  , tlDataBeats)
  val (nr_cnt, nr_finish) =
    Counter((io.tl_out.grant.fire() && ((read_multiple_data   && is_read && acq_data_process) || (mem_receive_tag) )), tlDataBeats)


  // signal to handler allocator
  io.rdy := (state===s_idle)
  io.tl_acq_match := tag_out === Cat(tl_acq.client_id, tl_acq.client_xact_id) && !io.rdy
  io.tl_rel_match := tag_out === Cat(tl_rel.client_id, tl_rel.client_xact_id) && !io.rdy


  io.tl.acquire.ready := is_acq && acq_data_process && (io.tl_out.acquire.fire())
  io.tl.release.ready := !is_acq && acq_data_process && io.tl_out.acquire.fire()

  //AQUIRE/ RELEASE RECEIVER
  when((state===s_idle) && io.tl.acquire.valid && !io.tl.release.valid) { // release take priority
    write_multiple_data := tl_acq.hasMultibeatData()
    read_multiple_data := !tl_acq.isBuiltInType() || tl_acq.isBuiltInType(Acquire.getBlockType)
    is_read := tl_acq.isBuiltInType() || !tl_acq.hasData() //shouold be &&
    is_write := tl_acq.isBuiltInType() && tl_acq.hasData()
    is_read_precess := tl_acq.isBuiltInType() || !tl_acq.hasData() //should be &&
    is_write_process := tl_acq.isBuiltInType() && tl_acq.hasData()
    is_acq := Bool(true)
    is_builtin := tl_acq.isBuiltInType()
    tag_out := Cat(tl_acq.client_id, tl_acq.client_xact_id)
    addr_out := io.phy_addr_acq
    len_out := Mux(!tl_acq.isBuiltInType() || !tl_acq.isSubBlockType(), UInt(tlDataBeats-1), UInt(0))
    size_out := Mux(!tl_acq.isBuiltInType() || !tl_acq.isSubBlockType() || tl_acq.hasData(),
      bytesToXSize(UInt(tlDataBytes)),
      opSizeToXSize(tl_acq.op_size()))
    g_type_out := Mux(tl_acq.isBuiltInType(), tl_acq.getBuiltInGrantType(), UInt(0)) // assume MI or MEI
  }

  when((state === s_idle) && io.tl.release.valid) {
    write_multiple_data := Bool(true)
    read_multiple_data := Bool(false)
    is_read := Bool(false)
    is_write := Bool(true)
    is_read_precess := Bool(false)
    is_write_process := Bool(true)
    is_builtin := Bool(true)
    tag_out := Cat(tl_rel.client_id, tl_rel.client_xact_id)
    addr_out := io.phy_addr_rel
    len_out := UInt(tlDataBeats-1)
    size_out := bytesToXSize(UInt(tlDataBytes))
    g_type_out := Grant.voluntaryAckType

  }

  when(acq_data_done) {
    collect_acq_data := Bool(false)
  }

  when(collect_acq_data) {
    acq_data(acq_data_cnt) := Mux(is_acq, tl_acq.data, tl_rel.data)
  }

  // GRANT HANDLER

  //Default Valid strobe
  io.tl.grant.valid := Bool(false)
  when(gnt_enable)
  {
    when(gnt_data_done || is_write_process)
    {
      gnt_enable := Bool(false)
    }

    io.tl.grant.valid := Bool(true)
  }

  gnt_data := gnt_data.fromBits(tagUtil.insertTag(mem_acq_data_read.toBits, gnt_tag))

  tl_gnt := Mux(is_write_process ,
    Grant(
      dst = tag_out >> tlClientXactIdBits,
      is_builtin_type = Bool(true),
      g_type = g_type_out,
      client_xact_id = tag_out(tlClientXactIdBits-1,0),
      manager_xact_id = UInt(id)),
    Grant(
      dst = tag_out >> tlClientXactIdBits,
      is_builtin_type = is_builtin,
      g_type = g_type_out,
      client_xact_id = tag_out(tlClientXactIdBits-1,0),
      manager_xact_id = UInt(id),
      addr_beat = gnt_data_cnt,
      data = UInt(gnt_data(gnt_data_cnt))
    ))


  // write back tags
  mem_tag_data_write := mem_tag_data_write.fromBits(wb_data.toBits)

  //Original request is handled if data is fully written or read from nasti
  when((io.tl_out.grant.fire() && acq_data_process && is_write) || (nr_finish && acq_data_process && is_read))
  {
    acq_data_process := Bool(false)
    cmd_sent := Bool(false)
    is_acq := Bool(false)
    is_read := Bool(false)
    is_write := Bool(false)
  }

  //Handle data received by Grant (mem or tag)
  when(io.tl_out.grant.valid)
  {
    //io.tl_out.grant.ready := Bool(true)
    when(acq_data_process && is_read)
    {
      mem_acq_data_read(nr_cnt) := tagUtil.removeTag(io.tl_out.grant.bits.data)
    }

    when(mem_receive_tag)
    {
      mem_tag_data_read(nr_cnt) := tagUtil.removeTag(io.tl_out.grant.bits.data)
    }

  }


  //ClientTileLink Acquire Handling
  val acq_out_send_mem =
      PutBlock(tag_out, addr_out >> (tlBeatAddrBits + tlByteAddrBits),
               UInt(0), UInt(tagUtil.insertTag(acq_rel_input_data_without_tag)), //Insert tag again because converter has tag awareness too
               UInt(0))

  val acq_out_reveive_mem = GetBlock(tag_out, addr_out >> (tlBeatAddrBits + tlByteAddrBits), Bool(false))

  val acq_out_send_tag =
      PutBlock(tag_out, (tagAddrConv(addrFromTag(acq_repl_meta, acq_addr),
               tagIsDram(io.meta.resp.bits.tag) ).toUInt() << UInt(blockOffBits)) >> (tlBeatAddrBits + tlByteAddrBits),
               UInt(0), UInt(tagUtil.insertTag((mem_tag_data_write(mem_tag_data_write_cnt)))),  //Insert tag again because converter has tag awareness too
               UInt(0))

  val acq_out_reveive_tag =
      GetBlock(tag_out,
              (tagAddrConv(acq_addr, is_dram_address) <<  UInt(blockOffBits)).toUInt() >> (tlBeatAddrBits + tlByteAddrBits),
              Bool(false))

 //Default
  io.tl_out.acquire.bits := acq_out_reveive_mem
  //Decide which Aquire Type to use
   when(mem_send_tag) {
     io.tl_out.acquire.bits := acq_out_send_tag
   }.elsewhen(mem_receive_tag) {
     io.tl_out.acquire.bits := acq_out_reveive_tag
   }.otherwise {
     when(is_write_process)
     {
       io.tl_out.acquire.bits := acq_out_send_mem
     }.otherwise {
       io.tl_out.acquire.bits := acq_out_reveive_mem
     }
   }

  when(io.tl_out.acquire.fire())
  {
    cmd_sent := Bool(true)
  }

  io.tl_out.acquire.valid :=  (((io.tl.acquire.valid && is_acq) || (io.tl.release.valid && !is_acq)) && is_write) || mem_send_tag ||  //Write valids
                              (((acq_data_process && is_read) || mem_receive_tag ) && !cmd_sent) //Read valids

  io.tl_out.grant.ready := (is_write || mem_send_tag || (state === s_write_back_wait_b)) || //Write readys
                           (is_read || mem_receive_tag) //Read readys


  //----------------------meta interface
  io.meta.read.valid := Bool(false)
  io.meta.read.bits.id := UInt(id)
  io.meta.read.bits.tag := addrToTag(acq_addr, Bool(false), Bool(false))
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
  io.meta.write.bits.tag := addrToTag(acq_addr, Bool(false), is_dram_address)
  io.meta.write.bits.idx := addrToIndex(acq_addr)
  io.meta.write.bits.way_en := acq_way_en

  when(state === s_meta_write_refill) {
    io.meta.write.valid := Bool(true)
  }

  when(state === s_meta_write_hit) {
    io.meta.write.valid := Bool(true)
    io.meta.write.bits.tag := addrToTag(acq_addr, Bool(true), is_dram_address)
  }

  //----------------------data array interface
  // read for hit
  io.data.read.valid := Bool(false)
  io.data.read.bits.id := UInt(id)
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
  when(state === s_data_write_refill) {
    io.data.write.valid := Bool(true)
    io.data.write.bits.data := refill_data(refill_data_cnt)
    io.data.write.bits.addr := Cat(addrToIndex(acq_addr), refill_data_cnt)
    io.data.write.bits.wmask := SInt(-1)
  }


  //----------------------state machine
  switch (state) {
    is(s_idle) {
      cmd_sent := Bool(false)
      when(io.tl.acquire.valid || io.tl.release.valid) {
        if (tlDataBeats > 1)
          collect_acq_data := (tl_acq.isBuiltInType() && tl_acq.hasData()) || io.tl.release.valid
        acq_data_process := Bool(true)

        state := s_meta_read
      }
    }

    is(s_dummy_wait) {
      when(!acq_data_process) {
        state := s_gnt
      }
    }

    is(s_meta_read) {
      when(io.meta.read.ready) { state := s_meta_resp }
    }
    is(s_meta_resp) {
      when(io.meta.resp.valid) {
        state :=
          Mux(io.meta.resp.bits.hit,
            Mux(is_write_process, s_data_write_hit, s_data_read_hit),  // cache hit
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
      when(!acq_data_process)
      {
        when( mem_tag_data_write_done) {
          state := s_write_back_wait_b
        }
      }

    }

    is(s_write_back_wait_b)
    {
      when( io.tl_out.grant.fire()) {
        cmd_sent := Bool(false)
        state := s_mem_req
      }
    }

    is(s_mem_req) {
      when(!acq_data_process) { // ensure the original req sent
        when(nr_finish) { state := s_data_write_refill }
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
        state := Mux(is_write_process, s_data_write_hit, s_data_read_hit)
      }
    }
    is(s_meta_write_hit) {
      when(io.meta.write.ready) {state := s_gnt }
    }

    is(s_gnt) {
      when(!acq_data_process)
      {
        gnt_enable := Bool(true)
        state := s_busy
      }
    }

    is(s_busy) {
      when(!gnt_enable && !acq_data_process) {
        is_write_process := Bool(false)
        is_read_precess := Bool(false)
        state := s_idle
      }
    }
  }

}


// tag cache metadata array
class TagCacheMetadataArray extends TagCacheModule {
  val io = new TagCacheMetaRWIO().flip
  // the highest bit in the meta is the valid flag
  val meta_bits = tagCacheTagBits+2 + 1

  val metaArray = SeqMem(Vec(UInt(width = meta_bits), nWays), nSets)

  val replacer = new RandomReplacement(nWays)

  // reset initial process
  val rst_cnt = Reg(init=UInt(0, log2Up(nSets+1)))
  val rst = rst_cnt < UInt(nSets)
  val rst_0 = rst_cnt === UInt(0)
  when (rst) { rst_cnt := rst_cnt+UInt(1) }

  // write request
  val waddr = Mux(rst, rst_cnt, io.write.bits.idx)
  val wdata = Mux(rst, UInt(0), io.write.bits.tag).toBits
  val wmask = Mux(rst, SInt(-1), io.write.bits.way_en.toSInt()).toBools

  when (rst || (io.write.valid)) {
    metaArray.write(waddr, Vec.fill(nWays)(wdata), wmask)
  }

  // helpers
  def getTag(meta:Bits): Bits = meta(tagCacheTagBits-1,0)
  def isValid(meta:Bits): Bool = meta(tagCacheTagBits+1)
  def isDirty(meta:Bits): Bool = meta(tagCacheTagBits)

  // read from cache array
  val ctags = metaArray.read(io.read.bits.idx, io.read.valid).toBits
  val ctagArray = Vec((0 until nWays).map(i => ctags(UInt((i+1)*meta_bits - 1), UInt(i*meta_bits))))

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
  val wmask = io.write.bits.wmask.toBools

  val resp = (0 until nWays).map { w =>
    val array = SeqMem(nSets*refillCycles, Vec( tagRowBlocks, Bits(width = tagBlockTagBits )) )

    when (io.write.bits.way_en(w) && io.write.valid) {
      array.write(waddr, Vec(io.write.bits.data), wmask)
    }
      /*.elsewhen (io.read.bits.way_en(w) && io.read.valid) {
      reg_raddr := raddr
    }
    */
    array.read(raddr)
  }

  io.resp.valid := ShiftRegister(io.read.fire(), 1)
  io.resp.bits.id := ShiftRegister(io.read.bits.id, 1)
  io.resp.bits.data := Mux1H(ShiftRegister(io.read.bits.way_en, 1), resp).toBits()

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