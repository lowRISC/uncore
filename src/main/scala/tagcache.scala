// See LICENSE for license details.

package uncore
import Chisel._
import cde.{Parameters, Field}
import junctions._
import scala.math.{min, max}

case object TCMemTransactors extends Field[Int]
case object TCTagTransactors extends Field[Int]

trait HasTCParameters extends HasCoherenceAgentParameters
    with HasCacheParameters with HasTagParameters
{
  val uncached = true

  val nMemAcquireTransactors = p(TCMemTransactors)
  val nMemReleaseTransactors = if(uncached) 0 else 1
  val nMemTransactors = nMemReleaseTransactors + nMemAcquireTransactors
  val nTagTransactors = p(TCTagTransactors)
  val nTopMapBlocks = max(1, tgHelper.topSize.toInt / p(CacheBlockBytes))

  val refillCycles = outerDataBeats

  require(outerDataBits == rowBits)
  require(p(CacheBlockBytes) * tgBits / 8 <= rowBits) // limit the data size of tag operation to a row
  require(outerTLId == p(TLId))
}

abstract class TCModule(implicit val p: Parameters) extends Module with HasTCParameters
abstract class TCBundle(implicit val p: Parameters) extends ParameterizedBundle()(p) with HasTCParameters

trait HasTCId extends HasTCParameters {
  val id = UInt(width = log2Up(nMemTransactors + nTagTransactors + 1))
}
trait HasTCTagFlag   extends HasTCParameters { val tcnt = UInt(width = log2Up(outerDataBeats+1)) }   // counting the rows with tags
trait HasTCTag       extends HasTCParameters { val tag  = UInt(width = tagBits) }
trait HasTCHit       extends HasTCParameters { val hit  = Bool() }

class TCIncoherence(implicit p: Parameters) extends CoherenceMetadata()(p)
class TCMetadata(implicit p: Parameters) extends Metadata()(p) with HasTCParameters with HasTCTagFlag
{
  val coh = new TCIncoherence
  val state = UInt(width=2) // MSI (0=invalid, 1=clean, 3=dirty)
  def isValid(dummy:Int=0) = state =/= TCMetadata.Invalid
  override def cloneType = new TCMetadata().asInstanceOf[this.type]
}

object TCMetadata {
  def apply(tag:UInt, tcnt:UInt, state: UInt)(implicit p: Parameters) = {
    val meta = Wire(new TCMetadata)
    meta.tag := tag
    meta.state := state
    meta.tcnt := tcnt
    meta
  }
  def Invalid = UInt("b00")
  def Clean = UInt("b01")
  def Dirty = UInt("b11")
  def onReset(implicit p: Parameters) = TCMetadata(UInt(0), UInt(0), UInt(0))(p)
}

class TCMetaReadReq(implicit p: Parameters) extends MetaReadReq()(p) with HasTCTag with HasTCId
class TCMetaWriteReq(implicit p: Parameters) extends MetaWriteReq[TCMetadata](new TCMetadata)(p)
class TCMetaReadResp(implicit p: Parameters) extends TCBundle()(p) with HasTCId with HasTCHit
{
  val meta = new TCMetadata
  val way_en = UInt(width = nWays)
}

class TCMetaIO(implicit p: Parameters) extends TCBundle()(p)
{
  val read = Decoupled(new TCMetaReadReq)
  val write = Decoupled(new TCMetaWriteReq)
  val resp = Valid(new TCMetaReadResp).flip
  override def cloneType = new TCMetaIO().asInstanceOf[this.type]
}

trait HasTCRow extends HasTCParameters  { val row  = UInt(width = log2Up(refillCycles)) }
trait HasTCData extends HasTCParameters { val data = UInt(width = rowBits) }
trait HasTCByteMask extends HasTCParameters { val mask = UInt(width = rowBytes) }
trait HasTCBitMask extends HasTCParameters { val mask = UInt(width = rowBits) }
trait HasTCAddr extends HasTCParameters { val addr = UInt(width=p(PAddrBits)) }

class TCDataReadReq(implicit p: Parameters) extends TCBundle()(p) with HasTCId with HasTCRow
{
  val idx = Bits(width = idxBits)
  val way_en = Bits(width = nWays)
}
class TCDataWriteReq(implicit p: Parameters) extends TCDataReadReq()(p) with HasTCData with HasTCByteMask
class TCDataReadResp(implicit p: Parameters) extends TCBundle()(p)
    with HasTCId with HasTCData

class TCDataIO(implicit p: Parameters) extends TCBundle()(p) {
  val read = Decoupled(new TCDataReadReq)
  val write = Decoupled(new TCDataWriteReq)
  val resp = Valid(new TCDataReadResp).flip
  override def cloneType = new TCDataIO().asInstanceOf[this.type]
}

class TCWBReq(implicit p: Parameters) extends TCDataReadReq()(p) with HasTCTag
class TCWBResp(implicit p: Parameters) extends TCBundle()(p) with HasTCId
class TCWBIO(implicit p: Parameters) extends TCBundle()(p) {
  val req = Decoupled(new TCWBReq)
  val resp = Valid(new TCWBResp).flip
  override def cloneType = new TCWBIO().asInstanceOf[this.type]
}

object TCTagOp {
  def nBits = 3
  def R     = UInt("b000") // read if hit
  def U     = UInt("b010") // unlock the line
  def F     = UInt("b001") // force a read and fetch if miss
  def FL    = UInt("b011") // force a read and lock it
  def W     = UInt("b100") // write a line
  def C     = UInt("b101") // create an empty line
  def CL    = UInt("b111") // create an empty line and lock it
  def I     = UInt("b110") // invalidate a line
  def isWrite(t:UInt) =  t(2)
  def isCreate(t:UInt) = t(2) && t(0)
  def isLock(t:UInt) = t(1) && t(0)
}

class TCTagRequest(implicit p: Parameters) extends TCBundle()(p)
    with HasTCId with HasTCData with HasTCBitMask with HasTCAddr
{
  val op   = UInt(width=TCTagOp.nBits)
}

class TCTagResp(implicit p: Parameters) extends TCBundle()(p) with HasTCId
    with HasTCHit with HasTCData with HasTCTagFlag

class TCTagXactIO(implicit p: Parameters) extends TCBundle()(p) {
  val req = Decoupled(new TCTagRequest)
  val resp = Valid(new TCTagResp).flip
}

class TCTagLock(implicit p: Parameters) extends TCBundle()(p) with HasTCId
{
  val addr = UInt(width=log2Up(tgHelper.map0Size)-tgHelper.blockOffBits) // only compare the offset after remove map0 base addr
  val lock = Bool() // lock or unlock
}

object TCTagLock {
  def apply()(implicit p: Parameters):TCTagLock = {
    val lock = Wire(new TCTagLock)
    lock.lock := Bool(false)
    lock
  }
}

////////////////////////////////////////////////////////////////
// tag cache metadata array

class TCMetadataArray(implicit p: Parameters) extends TCModule()(p) {
  val io = new TCMetaIO().flip

  val replacer = new RandomReplacement(nWays)
  val ren = io.read.fire()
  val onReset = () => TCMetadata.onReset
  val meta = Module(new MetadataArray[TCMetadata](onReset))
  meta.io.read <> io.read
  meta.io.read.bits.way_en := ~UInt(0,nWays)
  meta.io.write <> io.write

  val s1_read_valid = Reg(next = ren)
  val s1_id         = RegEnable(io.read.bits.id, ren)
  val s1_tag        = RegEnable(io.read.bits.tag, ren)
  val s1_match_way  = Vec(meta.io.resp.map(m => m.tag === s1_tag && m.isValid())).toBits

  val s2_match_way  = RegEnable(s1_match_way, s1_read_valid)
  val s2_repl_way   = RegEnable(replacer.way, s1_read_valid)
  val s2_hit        = s2_match_way.orR
  val s2_meta       = RegEnable(meta.io.resp, s1_read_valid)

  when(s1_read_valid && !s1_match_way.orR) {replacer.miss}

  io.resp.valid         := Reg(next = s1_read_valid)
  io.resp.bits.id       := RegEnable(s1_id, s1_read_valid)
  io.resp.bits.hit      := s2_hit
  io.resp.bits.way_en   := Mux(s2_hit, s2_match_way, UIntToOH(s2_repl_way))
  io.resp.bits.meta     := Mux(s2_hit, s2_meta(OHToUInt(s2_match_way)), s2_meta(s2_repl_way))
}

////////////////////////////////////////////////////////////////
// tag cache data array

class TCDataArray(implicit p: Parameters) extends TCModule()(p) {
  val io = new TCDataIO().flip

  val array = SeqMem(nWays*nSets*refillCycles, Vec(rowBits/8, Bits(width=8)))
  val ren = io.read.fire()
  val raddr = Cat(OHToUInt(io.read.bits.way_en), io.read.bits.idx, io.read.bits.row)
  val waddr = Cat(OHToUInt(io.write.bits.way_en), io.write.bits.idx, io.write.bits.row)
  val wdata = Vec.tabulate(rowBytes)(i => io.write.bits.data(8*(i+1)-1,8*i))
  val wmask = io.write.bits.mask.toBools
  when (io.write.valid) { array.write(waddr, wdata, wmask) }

  val s1_data       = array.read(raddr, ren).toBits
  val s1_read_valid = Reg(next = ren)
  val s1_id         = RegEnable(io.read.bits.id, ren)

  io.resp.valid     := Reg(next = s1_read_valid)
  io.resp.bits.id   := RegEnable(s1_id, s1_read_valid)
  io.resp.bits.data := RegEnable(s1_data, s1_read_valid)

  io.read.ready     := !io.write.valid
  io.write.ready    := Bool(true)
}

////////////////////////////////////////////////////////////////
// Tag Cache Writeback Unit

class TCWritebackUnit(id: Int)(implicit p: Parameters) extends TCModule()(p) with HasTileLinkParameters {
  val io = new Bundle {
    val xact = new TCWBIO().flip
    val data = new TCDataIO
    val tl   = new ClientUncachedTileLinkIO()
    val wb_addr = UInt(OUTPUT, width=tlBlockAddrBits)
    val addr_block = Vec(nTagTransactors, UInt(width=tlBlockAddrBits)).asInput
    val addr_match = Vec(nTagTransactors, Bool()).asOutput
    val mtActive = UInt(INPUT, nMemTransactors)
  }

  // check          empty line, check whether it is safe to avoid writeback
  // read           read non-empty line from data array
  // write          write line to memory

  val s_IDLE :: s_READ :: s_READ_DONE :: s_WRITE :: s_GNT :: Nil = Enum(UInt(), 5)
  val state = Reg(init = s_IDLE)

  val data_buffer = Reg(init=Vec.fill(refillCycles)(UInt(0, rowBits)))
  val (read_cnt, read_done) = Counter(io.data.read.fire() && state === s_READ, refillCycles)
  val (write_cnt, write_done) = Counter(io.data.resp.valid, refillCycles)
  val (tl_cnt, tl_done) = Counter(state === s_WRITE && io.tl.acquire.fire(), outerDataBeats)

  val xact = RegEnable(io.xact.req.bits, io.xact.req.fire())
  io.xact.req.ready := state === s_IDLE
  io.xact.resp.bits.id := xact.id
  io.xact.resp.valid := tl_done

  io.data.read.valid := state === s_READ
  io.data.read.bits := xact
  io.data.read.bits.id := UInt(id)
  io.data.read.bits.row := read_cnt

  io.data.write.valid := Bool(false)

  when(state === s_IDLE) {
    data_buffer := Vec.fill(refillCycles)(UInt(0, rowBits))
  }
  when(io.data.resp.valid) {
    data_buffer(write_cnt) := io.data.resp.bits.data
  }

  io.tl.acquire.valid := state === s_WRITE
  val tl_addr:UInt = Cat(xact.tag, xact.idx, tl_cnt, UInt(0, rowOffBits))
  io.tl.acquire.bits :=
    PutBlock(
      UInt(nMemTransactors+id),
      tlGetBlockAddr(tl_addr),
      tlGetBeatAddr(tl_addr),
      data_buffer(tl_cnt)
    )

  io.tl.grant.ready := state === s_GNT

  when(state === s_IDLE && io.xact.req.valid) {
    state := s_READ
  }
  when(state === s_READ && read_done) {
    state := s_READ_DONE
  }
  when(state === s_READ_DONE && write_done) {
    state := s_WRITE
  }
  when(state === s_WRITE && tl_done) {
    state := s_GNT
  }
  when(state === s_GNT && io.tl.grant.valid) {
    state := s_IDLE
  }

  when(io.xact.req.fire()) {
    printf(s"TagWB$id: (%d) Write back 0x%x\n", xact.id, Cat(xact.tag, xact.idx, UInt(0, tlBlockOffsetBits)))
  }
}

////////////////////////////////////////////////////////////////
// Tag Transaction Tracker

class TCTagXactTracker(id: Int)(implicit p: Parameters) extends TCModule()(p) with HasTileLinkParameters {
  val io = new Bundle {
    val xact = new TCTagXactIO().flip
    val meta = new TCMetaIO
    val data = new TCDataIO
    val wb   = new TCWBIO
    val tl   = new ClientUncachedTileLinkIO()
    val lock = Decoupled(new TCTagLock)
  }

  // IDLE:      idle, ready for new request
  // MR:        read metadata
  // DR:        read a row of data
  // DWR:       write a row of data
  // MW:        write metadata
  // WB:        write back a line
  // F:         fetch a line
  // DWB:       write a line of data
  // L:         lock or unlock


  val s_IDLE :: s_MR :: s_DR :: s_DWR :: s_MW :: s_WB :: s_F :: s_DWB :: s_L :: Nil = Enum(UInt(), 9)
  val state      = Reg(init=s_IDLE)
  val state_next = Wire(init=state)
  state := state_next

  // internal signals
  val xact = RegEnable(io.xact.req.bits, io.xact.req.fire())
  val byte_mask = Vec((0 until rowBytes).map(i => xact.mask(i*8+7, i*8).orR)).toBits
  val idx = xact.addr(idxBits+blockOffBits-1, blockOffBits)
  val row = xact.addr(blockOffBits-1,rowOffBits)

  // break meta as workaround, do not really undersatnd the NO DEFAULT WIRE faults here
  // val meta = RegEnable(io.meta.resp.bits, io.meta.resp.valid)
  val hit        = RegEnable(io.meta.resp.bits.hit,    io.meta.resp.valid)
  val way_en     = RegEnable(io.meta.resp.bits.way_en, io.meta.resp.valid)
  val meta_tag   = Reg(init=UInt(0,tagBits))
  val meta_state = Reg(init=TCMetadata.Invalid)
  val meta_tcnt  = Reg(init=UInt(0,log2Up(outerDataBeats+1)))
  when(io.meta.resp.valid) {
    meta_tag   := io.meta.resp.bits.meta.tag
    meta_state := io.meta.resp.bits.meta.state
    meta_tcnt  := io.meta.resp.bits.meta.tcnt
  }

  val addrTag = xact.addr >> untagBits
  val (data_cnt, data_done) = Counter(state === s_DWB && io.data.write.fire(), refillCycles)
  val data_buf = Reg(Vec(outerDataBeats, UInt(width=outerDataBits)))
  val (fetch_cnt, fetch_done) = Counter(io.tl.grant.fire(), outerDataBeats)
  val update_meta = Reg(init = Bool(false))
  val req_sent = Reg(init = Bool(false))

  // transaction
  io.xact.req.ready := state === s_IDLE
  io.xact.resp.bits.id := xact.id
  io.xact.resp.bits.hit := xact.op =/= TCTagOp.R || RegEnable(io.meta.resp.bits.hit, io.meta.resp.valid)
  io.xact.resp.bits.tcnt := meta_tcnt       // only make sense when hit
  io.xact.resp.bits.data := data_buf(row)   // only make sense for R and F when hit
  io.xact.resp.valid := state === s_L && state_next === s_IDLE
  when(io.meta.resp.valid) { data_buf(row) := UInt(0) }
  when(io.data.resp.valid) { data_buf(row) := io.data.resp.bits.data }

  // metadata read
  io.meta.read.bits.id := UInt(id)
  io.meta.read.bits.idx := idx
  io.meta.read.bits.tag := addrTag
  io.meta.read.valid := state === s_MR && !req_sent

  // metadata write
  io.meta.write.bits.idx := idx
  io.meta.write.bits.way_en := way_en
  io.meta.write.bits.data := TCMetadata(addrTag, meta_tcnt, meta_state)
  io.meta.write.valid := state === s_MW

  // data array read
  io.data.read.bits.id := UInt(id)
  io.data.read.bits.row := row
  io.data.read.bits.idx := idx
  io.data.read.bits.way_en := way_en
  io.data.read.valid := state === s_DR && !req_sent

  // data array write
  io.data.write.bits.id := UInt(id)
  io.data.write.bits.row := Mux(state === s_DWB, data_cnt, row)
  io.data.write.bits.idx := idx
  io.data.write.bits.way_en := way_en
  io.data.write.bits.data := Mux(state === s_DWB, data_buf(data_cnt), data_buf(row))
  io.data.write.bits.mask := Mux(state === s_DWB, ~UInt(0,rowBytes), byte_mask)
  io.data.write.valid := state === s_DWR || state === s_DWB

  // write-back
  val wb_sent = Reg(init=Bool(false))
  io.wb.req.bits.id := UInt(id)
  io.wb.req.bits.row := row
  io.wb.req.bits.idx := idx
  io.wb.req.bits.way_en := way_en
  io.wb.req.bits.tag := meta_tag
  io.wb.req.valid := state === s_WB && !wb_sent
  when(state =/= state_next && state_next===s_WB) {
    wb_sent := Bool(false)
  }.elsewhen(io.wb.req.ready) {
    wb_sent := Bool(true)
  }

  // fetch
  io.tl.acquire.bits := GetBlock(UInt(id), tlGetBlockAddr(xact.addr))
  io.tl.acquire.valid := state === s_F && !req_sent && !TCTagOp.isCreate(xact.op)
  io.tl.grant.ready := state === s_F && req_sent

  // req_sent update
  when(state =/= state_next) {
    req_sent := Bool(false)
  }.elsewhen(io.meta.read.fire() || io.data.read.fire() || io.tl.acquire.fire()) {
    req_sent := Bool(true)
  }

  // lock
  io.lock.bits.id := xact.id
  io.lock.bits.addr := xact.addr >> tgHelper.blockOffBits
  io.lock.bits.lock := TCTagOp.isLock(xact.op)
  io.lock.valid := state === s_L && tgHelper.is_map(xact.addr)

  // data array update function
  def beat_data_update(tl_data:UInt, index:UInt) = {
    Mux(index === tlGetBeatAddr(xact.addr) && xact.op === TCTagOp.W, (tl_data & ~xact.mask) | xact.data, tl_data)
  }

  // update meta and data
  when(state === s_DR && io.data.resp.valid) {
    data_buf(row) := (xact.data & xact.mask) | (io.data.resp.bits.data & ~xact.mask)
    when ((xact.data & xact.mask).orR && !io.data.resp.bits.data.orR) {
      assert(!meta_tcnt.andR, "add a tag in a full line is impossible!")
      meta_tcnt := meta_tcnt + UInt(1)
    }
    when (!(xact.data & xact.mask).orR && io.data.resp.bits.data.orR) {
      assert(meta_tcnt.orR, "clear a tag in an empty line is impossible!")
      meta_tcnt := meta_tcnt - UInt(1)
    }
  }

  when(state === s_MR && state_next === s_MW) {
    meta_state := TCMetadata.Invalid
  }

  when(state =/= state_next && state_next === s_F) {
    when(TCTagOp.isCreate(xact.op)) { // create
      (0 until outerDataBeats).foreach(i => {
        data_buf(i) := beat_data_update(UInt(0,outerDataBits), UInt(i))
      })
      meta_tcnt  := UInt(0)
      meta_state := TCMetadata.Dirty // create always ends up in dirty
    }.otherwise{
      meta_tcnt  := UInt(0)
      meta_state := TCMetadata.Clean
    }
  }

  when(state === s_F && io.tl.grant.valid) {
    val m_update_data = beat_data_update(io.tl.grant.bits.data, fetch_cnt)
    data_buf(fetch_cnt) := m_update_data
    meta_tcnt := meta_tcnt + Mux(m_update_data =/= UInt(0), UInt(1), UInt(0))
    meta_state := Mux(m_update_data =/= io.tl.grant.bits.data, TCMetadata.Dirty, meta_state)
  }

  when(state === s_DWR && io.data.write.ready) {
    meta_state := TCMetadata.Dirty
  }

  // state machine
  when(state === s_IDLE && io.xact.req.valid) {
    state_next := s_MR
  }
  when(state === s_MR && io.meta.resp.valid) {
    when(xact.op === TCTagOp.U) {
      // no need to read data for unlock or invalidation
      state_next := s_L
    }.elsewhen(io.meta.resp.bits.hit) {
      state_next := s_DR
      when(!TCTagOp.isWrite(xact.op) && io.meta.resp.bits.meta.tcnt === UInt(0)) {
        // find an empty top map line
        assert(tgHelper.is_top(xact.addr), "only top map lines can be empty and cached at any time!")
        state_next := s_L
      }
      when(xact.op === TCTagOp.I) {
        state_next := s_MW
      }
    }.otherwise {
      // write back dirty lines
      when(xact.op === TCTagOp.I || xact.op === TCTagOp.R) {
        state_next := s_L
      }.otherwise {
        state_next := Mux(io.meta.resp.bits.meta.state === TCMetadata.Dirty, s_WB, s_F)
      }
    }
  }
  when(state === s_DR && io.data.resp.valid) {
    state_next := Mux(TCTagOp.isWrite(xact.op), s_DWR, s_L)
  }
  when(state === s_DWR && io.data.write.ready) {
    state_next := s_MW
  }
  when(state === s_WB && io.wb.resp.valid) {
    state_next := s_F
  }
  when(state === s_F && (fetch_done || TCTagOp.isCreate(xact.op))) {
    state_next := s_DWB
  }
  when(state === s_DWB && data_done) {
    state_next := s_MW
  }
  when(state === s_MW && io.meta.write.ready) {
    state_next := s_L
  }
  when(state === s_L && io.lock.ready) {
    state_next := s_IDLE
  }

  // run-time checks
  assert(!io.meta.resp.valid || !TCTagOp.isCreate(xact.op) || !io.meta.resp.bits.hit,
    "a tag cache create transaction should always miss in cache!")
  assert(state === state_next || (data_cnt === UInt(0) || data_done) && (fetch_cnt === UInt(0) || fetch_done),
    "counters should return to zero when state changes!")

  // report log
  when(io.xact.resp.valid) {
    when(xact.op === TCTagOp.R && io.xact.resp.bits.hit) {
      printf(s"TagXact$id: (%d) Read 0x%x => %x\n", xact.id, xact.addr, io.xact.resp.bits.data)
    }
    when(xact.op === TCTagOp.R && !io.xact.resp.bits.hit) {
      printf(s"TagXact$id: (%d) Read 0x%x miss\n", xact.id, xact.addr)
    }
    when(xact.op === TCTagOp.U) {
      printf(s"TagXact$id: (%d) Unlock 0x%x\n", xact.id, xact.addr)
    }
    when(xact.op === TCTagOp.F) {
      printf(s"TagXact$id: (%d) FetchRead 0x%x => %x\n", xact.id, xact.addr, io.xact.resp.bits.data)
    }
    when(xact.op === TCTagOp.FL) {
      printf(s"TagXact$id: (%d) FetchRead and lock 0x%x => %x\n", xact.id, xact.addr, io.xact.resp.bits.data)
    }
    when(xact.op === TCTagOp.W) {
      printf(s"TagXact$id: (%d) Write 0x%x <= %x using mask %x\n", xact.id, xact.addr, xact.data, xact.mask)
    }
    when(xact.op === TCTagOp.C) {
      printf(s"TagXact$id: (%d) Create 0x%x\n", xact.id, xact.addr)
    }
    when(xact.op === TCTagOp.CL) {
      printf(s"TagXact$id: (%d) Create and lock 0x%x\n", xact.id, xact.addr)
    }
    when(xact.op === TCTagOp.I) {
      printf(s"TagXact$id: (%d) Invalidate 0x%x\n", xact.id, xact.addr)
    }
  }
}

////////////////////////////////////////////////////////////////
// Memory Transaction Tracker

class TCMemXactTracker(id: Int)(implicit p: Parameters) extends TCModule()(p)
    with HasDataBeatCounters
{
  val io = new Bundle {
    val inner = new ManagerTileLinkIO()(p.alterPartial({case TLId => p(InnerTLId)}))
    val outer = new ClientUncachedTileLinkIO()(p.alterPartial({case TLId => p(OuterTLId)}))
    val tc = new TCTagXactIO
    val tl_block = Bool(OUTPUT)                                    // start to block other tl transactions
    val tl_addr_block = UInt(OUTPUT, width=inner.tlBlockAddrBits)  // for cache line comparison
    val tm0_addr = UInt(OUTPUT, width=inner.tlBlockAddrBits)
    val tt_addr = UInt(OUTPUT, width=inner.tlBlockAddrBits)
    val create_tm0 = Bool(OUTPUT)                                  // active for create a TM0 block
    val create_tt = Bool(OUTPUT)                                   // active for create a TT block
    val create_rdy = Bool(INPUT)
  }
  def inner: ManagerTileLinkIO = io.inner
  def outer: ClientUncachedTileLinkIO = io.outer
  def innerPM : Parameters = p.alterPartial({case TLId =>innerTLId})
  def outerPM : Parameters = p.alterPartial({case TLId =>outerTLId})
  val coh = ManagerMetadata.onReset(innerPM)

  val tc_xact_rw        = Wire(Bool()) // r:0 w:1
  val tc_xact_mem_data  = Reg(UInt(width = tgHelper.cacheBlockTagBits))
  val tc_xact_mem_mask  = Wire(UInt(width = tgHelper.cacheBlockTagBits))
  val tc_xact_mem_addr  = Wire(UInt(width = tgHelper.cacheBlockTagBits))
  val tc_xact_tt_tag1   = Reg(Bool()) // the target tag is set
  val tc_xact_tt_tagN   = Reg(Bool()) // there are other tags
  val tc_xact_tm0_tag1  = Reg(Bool())
  val tc_xact_tm0_tagN  = Reg(Bool())
  val tc_xact_tm1_tag1  = Reg(Bool())

  def write_tc_xact_data(data:UInt, mask:UInt):Unit = { tc_xact_mem_data := (tc_xact_mem_data & ~mask) | (data & mask) }

  // ------------ tag cache state machine states -------------- //
  // IDLE:          ready fro new memory transactions
  // TTR:           read tag table
  // TM0R:          read tag map 0
  // TM1F:          force read of tag map 1
  // TM0F:          force read of tag map 0
  // TTF:           force read of tag table
  // TM1L:          lock tag map 1
  // TM0L:          lock tag map 0, does not really lock it but to force read or create
  // TTL:           lock tag table, does not really lock it but to force read or create
  // TTW:           invalidate or update tag table if necessary
  // TM0W:          invalidate or update tag map 0 if necessary
  // TM1W:          update tag map 1 if necessary, or just unlock it

  val ts_IDLE :: ts_TTR :: ts_TM0R :: ts_TM1F :: ts_TM0F :: ts_TTF :: ts_TM1L :: ts_TM0L :: ts_TTL :: ts_TTW :: ts_TM0W :: ts_TM1W :: Nil = Enum(UInt(), 12)
  val tc_state      = Reg(init=ts_IDLE)
  val tc_state_next = Wire(init=tc_state)
      tc_state := tc_state_next

  val tc_req_valid = Wire(Bool())

  val tc_tt_addr    = tgHelper.pa2tta(tc_xact_mem_addr)
  val tc_tt_byte_index = tgHelper.pa2ttr(tc_xact_mem_addr, rowOffBits)
  val tc_tt_rdata   = Wire(UInt(width = tgHelper.cacheBlockTagBits))
      tc_tt_rdata  := io.tc.resp.bits.data >> (tc_tt_byte_index << 3)
  val tc_tt_wdata   = Wire(UInt(width = rowBits))
      tc_tt_wdata  := tc_xact_mem_data << (tc_tt_byte_index << 3)
  val tc_tt_wmask   = Wire(UInt(width = rowBits))
      tc_tt_wmask  := tc_xact_mem_mask << (tc_tt_byte_index << 3)
  val tc_tm0_addr   = tgHelper.pa2tm0a(tc_xact_mem_addr)
  val tc_tm0_bit_index = tgHelper.pa2tm0b(tc_xact_mem_addr, rowOffBits)
  val tc_tm0_rdata  = io.tc.resp.bits.data(tc_tm0_bit_index)
  val tc_tm0_wdata  = Wire(UInt(width = rowBits))
      tc_tm0_wdata := (tc_xact_tt_tag1 || tc_xact_tt_tagN) << tc_tm0_bit_index
  val tc_tm0_wmask  = Wire(UInt(width = rowBits))
      tc_tm0_wmask := UInt(1) << tc_tm0_bit_index
  val tc_tm1_addr   = tgHelper.pa2tm1a(tc_xact_mem_addr)
  val tc_tm1_bit_index = tgHelper.pa2tm1b(tc_xact_mem_addr, rowOffBits)
  val tc_tm1_rdata  = io.tc.resp.bits.data(tc_tm1_bit_index)
  val tc_tm1_wdata  = Wire(UInt(width = rowBits))
      tc_tm1_wdata := (tc_xact_tt_tag1 || tc_xact_tt_tagN || tc_xact_tm0_tagN) << tc_tm1_bit_index
  val tc_tm1_wmask  = Wire(UInt(width = rowBits))
      tc_tm1_wmask := UInt(1) << tc_tm1_bit_index // check generated verilog

  // tag transaction request
  val tc_req_sent = Reg(init=Bool(false))
  io.tc.req.valid     := tc_state =/= ts_IDLE && !tc_req_sent
  io.tc.req.bits.id   := UInt(id)
  io.tc.req.bits.data := UInt(0)
  io.tc.req.bits.mask := UInt(0)
  io.tc.req.bits.addr := UInt(0)
  io.tc.req.bits.op   := UInt(0)
  when(tc_state =/= tc_state_next) {
    tc_req_sent := Bool(false)
  }.elsewhen(io.tc.req.ready) {
    tc_req_sent := Bool(true)
  }

  switch(tc_state) {
    is(ts_TTR) {
      io.tc.req.bits.addr := tc_tt_addr
      io.tc.req.bits.op   := TCTagOp.R
    }
    is(ts_TM0R) {
      io.tc.req.bits.addr := tc_tm0_addr
      io.tc.req.bits.op   := TCTagOp.R
    }
    is(ts_TM1F) {
      io.tc.req.bits.addr := tc_tm1_addr
      io.tc.req.bits.op   := TCTagOp.F
    }
    is(ts_TM0F) {
      io.tc.req.bits.addr := tc_tm0_addr
      io.tc.req.bits.op   := TCTagOp.F
    }
    is(ts_TTF) {
      io.tc.req.bits.addr := tc_tt_addr
      io.tc.req.bits.op   := TCTagOp.F
    }
    is(ts_TM1L) {
      io.tc.req.bits.addr := tc_tm1_addr
      io.tc.req.bits.op   := TCTagOp.FL
    }
    is(ts_TM0L) {
      io.tc.req.bits.addr := tc_tm0_addr
      io.tc.req.bits.op   := Mux(tc_xact_tm1_tag1, TCTagOp.FL, TCTagOp.CL)
    }
    is(ts_TTL) {
      io.tc.req.bits.addr := tc_tt_addr
      io.tc.req.bits.op   := Mux(tc_xact_tm0_tag1, TCTagOp.F, TCTagOp.C)
    }
    is(ts_TTW) {
      io.tc.req.bits.data := tc_tt_wdata
      io.tc.req.bits.mask := tc_tt_wmask
      io.tc.req.bits.addr := tc_tt_addr
      io.tc.req.bits.op   := Mux(!tc_xact_tt_tag1 && !tc_xact_tt_tagN, TCTagOp.I, TCTagOp.W)
    }
    is(ts_TM0W) {
      io.tc.req.bits.data := tc_tm0_wdata
      io.tc.req.bits.mask := tc_tm0_wmask
      io.tc.req.bits.addr := tc_tm0_addr
      io.tc.req.bits.op   := Mux((tc_xact_tt_tag1 || tc_xact_tt_tagN) === tc_xact_tm0_tag1, TCTagOp.U,
                             Mux(!tc_xact_tt_tag1 && !tc_xact_tt_tagN && !tc_xact_tm0_tagN, TCTagOp.I, TCTagOp.W))
    }
    is(ts_TM1W) {
      io.tc.req.bits.data := tc_tm1_wdata
      io.tc.req.bits.mask := tc_tm1_wmask
      io.tc.req.bits.addr := tc_tm1_addr
      io.tc.req.bits.op   := Mux((tc_xact_tt_tag1 || tc_xact_tt_tagN || tc_xact_tm0_tagN) === tc_xact_tm1_tag1, TCTagOp.U, TCTagOp.W)
    }
  }

  // tag transaction response
  when(io.tc.resp.valid) {
    switch(tc_state) {
      is(ts_TTR) {
        when(io.tc.resp.bits.hit) {
          write_tc_xact_data(tc_tt_rdata, Mux(tc_xact_rw, ~tc_xact_mem_mask, ~UInt(0, tgHelper.cacheBlockTagBits)))
        }
      }
      is(ts_TM0R) {
        when(io.tc.resp.bits.hit && !tc_tm0_rdata) {
          write_tc_xact_data(UInt(0), Mux(tc_xact_rw, ~tc_xact_mem_mask, ~UInt(0, tgHelper.cacheBlockTagBits)))
        }
      }
      is(ts_TM1F) {
        when(io.tc.resp.bits.hit && !tc_tm1_rdata) {
          write_tc_xact_data(UInt(0), Mux(tc_xact_rw, ~tc_xact_mem_mask, ~UInt(0, tgHelper.cacheBlockTagBits)))
        }
      }
      is(ts_TM0F) {
        when(!tc_tm0_rdata) {
          write_tc_xact_data(UInt(0), Mux(tc_xact_rw, ~tc_xact_mem_mask, ~UInt(0, tgHelper.cacheBlockTagBits)))
        }
      }
      is(ts_TTF) {
        write_tc_xact_data(tc_tt_rdata, Mux(tc_xact_rw, ~tc_xact_mem_mask, ~UInt(0, tgHelper.cacheBlockTagBits)))
      }
      is(ts_TM1L) {
        tc_xact_tm1_tag1 := tc_tm1_rdata
      }
      is(ts_TM0L) {
        tc_xact_tm0_tag1 := tc_xact_tm1_tag1 && tc_tm0_rdata
        tc_xact_tm0_tagN := tc_xact_tm1_tag1 && (io.tc.resp.bits.tcnt > UInt(1) || (io.tc.resp.bits.data & ~tc_tm0_wmask) =/= UInt(0))
      }
      is(ts_TTL) {
        tc_xact_tt_tag1 := (tc_xact_mem_data & tc_xact_mem_mask) =/= UInt(0)
        tc_xact_tt_tagN := tc_xact_tm0_tag1 && (io.tc.resp.bits.tcnt > UInt(1) || (io.tc.resp.bits.data & ~tc_tt_wmask) =/= UInt(0))
      }
    }
  }

  // -------------- the shared state machine ----------------- //
  when(tc_state === ts_IDLE && tc_req_valid) {
    tc_state_next := ts_TTR
  }
  when(tc_state === ts_TTR && io.tc.resp.valid) {
    tc_state_next := Mux(!io.tc.resp.bits.hit, ts_TM0R,
                         Mux(tc_xact_rw && (tc_xact_mem_data & tc_xact_mem_mask) =/= (tc_tt_rdata & tc_xact_mem_mask),
                             ts_TM1L, ts_IDLE))
  }
  when(tc_state === ts_TM0R && io.tc.resp.valid) {
    tc_state_next := Mux(!io.tc.resp.bits.hit, ts_TM1F,
                         Mux(tc_tm0_rdata, ts_TTF,
                             Mux(tc_xact_rw && (tc_xact_mem_data & tc_xact_mem_mask) =/= UInt(0), ts_TM1L, ts_IDLE)))
  }
  when(tc_state === ts_TM1F && io.tc.resp.valid) {
    tc_state_next := Mux(tc_tm1_rdata, ts_TM0F,
                         Mux(tc_xact_rw && (tc_xact_mem_data & tc_xact_mem_mask) =/= UInt(0), ts_TM1L, ts_IDLE))
  }
  when(tc_state === ts_TM0F && io.tc.resp.valid) {
    tc_state_next := Mux(tc_tm0_rdata, ts_TTF,
                         Mux(tc_xact_rw && (tc_xact_mem_data & tc_xact_mem_mask) =/= UInt(0), ts_TM1L, ts_IDLE))
  }
  when(tc_state === ts_TTF && io.tc.resp.valid) {
    tc_state_next := Mux(tc_xact_rw && (tc_xact_mem_data & tc_xact_mem_mask) =/= (tc_tt_rdata & tc_xact_mem_data),
                         ts_TM1L, ts_IDLE)
  }
  when(tc_state === ts_TM1L && io.tc.resp.valid) {
    tc_state_next := ts_TM0L
  }
  when(tc_state === ts_TM0L && io.tc.resp.valid) {
    tc_state_next := ts_TTL
  }
  when(tc_state === ts_TTL && io.tc.resp.valid) {
    tc_state_next := ts_TTW
  }
  when(tc_state === ts_TTW && io.tc.resp.valid) {
    tc_state_next := ts_TM0W
  }
  when(tc_state === ts_TM0W && io.tc.resp.valid) {
    tc_state_next := ts_TM1W
  }
  when(tc_state === ts_TM1W && io.tc.resp.valid) {
    tc_state_next := ts_IDLE
  }
}

class TCMemReleaseTracker(id: Int)(implicit p: Parameters) extends TCMemXactTracker(id)(p) {

  // ------------ tag cache state machine states -------------- //
  val ms_IDLE :: ms_IREL :: ms_OACQ :: ms_IGNT :: Nil = Enum(UInt(), 4)
  val mt_state = Reg(init = ms_IDLE)

  require(inner.tlDataBits == outer.tlDataBits)
  require(inner.tlDataBeats == outer.tlDataBeats)
  val xact = Reg(new BufferedReleaseFromSrc()(innerPM))
  val tmask = Wire(Vec(inner.tlDataBeats, UInt(width=tgHelper.tagSize(inner.tlDataBits))))
  val tdata = Wire(Vec(inner.tlDataBeats, UInt(width=tgHelper.tagSize(inner.tlDataBits))))
  (0 until inner.tlDataBeats).foreach( i => {
    tmask(i) := UInt(0)
    tdata(i) := UInt(0)
  })

  val irel_done = connectIncomingDataBeatCounter(inner.release)
  val (oacq_cnt, oacq_done) = connectOutgoingDataBeatCounter(outer.acquire)

  // inner release
  when(inner.release.fire()) {
    xact.data_buffer(inner.release.bits.addr_beat) := inner.release.bits.data
  }
  inner.release.ready := (mt_state === ms_IDLE && tc_state === ts_IDLE) || mt_state === ms_IREL

  // output acquire
  outer.acquire.valid := mt_state === ms_OACQ
  outer.acquire.bits := PutBlock(
    client_xact_id = UInt(id),
    addr_block = xact.addr_block,
    addr_beat = oacq_cnt,
    data = xact.data_buffer(oacq_cnt)
  )(outerPM)

  // input grant
  inner.grant.valid := mt_state === ms_IGNT && outer.grant.valid
  inner.grant.bits := coh.makeGrant(xact)
  outer.grant.ready := mt_state === ms_IGNT && inner.grant.ready

  // tag
  when(inner.release.fire()) {
    tmask(inner.release.bits.addr_beat) := ~UInt(0,tgHelper.tagSize(inner.tlDataBits))
    tdata(inner.release.bits.addr_beat) := inner.release.bits.tag
    write_tc_xact_data(tdata.toBits, tmask.toBits)
  }
  tc_req_valid := mt_state === irel_done
  tc_xact_rw := Bool(true)
  tc_xact_mem_addr := xact.full_addr()
  tc_xact_mem_mask := ~UInt(0, tgHelper.cacheBlockTagBits)

  // tl conflicts
  // block memory side transactions
  io.tl_block := mt_state =/= ts_IDLE || tc_state =/= ts_IDLE
  io.tl_addr_block := xact.addr_block

  // ------------ memory tracker state machine ---------------- //
  when(mt_state === ms_IDLE && tc_state === ms_IDLE && inner.release.fire()) {
    xact := inner.release.bits
    mt_state := Mux(irel_done, ms_OACQ, ms_IREL)
  }
  when(mt_state === ms_IREL && irel_done) {
    mt_state := ms_OACQ
  }
  when(mt_state === ms_OACQ && oacq_done) {
    mt_state := Mux(xact.requiresAck(), ms_IGNT, ms_IDLE)
  }
  when(mt_state === ms_IGNT && inner.grant.fire()) {
    mt_state := ms_IDLE
  }

}

class TCMemAcquireTracker(id: Int)(implicit p: Parameters) extends TCMemXactTracker(id)(p) {

  // ------------ tag cache state machine states -------------- //
  val ms_IDLE :: ms_IACQ :: ms_OACQ :: ms_OGNT :: ms_IGNT :: ms_IFIN :: Nil = Enum(UInt(), 6)
  val mt_state = Reg(init = ms_IDLE)

  require(inner.tlDataBits == outer.tlDataBits)
  require(inner.tlDataBeats == outer.tlDataBeats)
  val xact  = Reg(new BufferedAcquireFromSrc()(innerPM))
  val xact_tag_buffer = Vec.fill(inner.tlDataBeats)(UInt(0,tgHelper.tagSize(inner.tlDataBits))).fromBits(tc_xact_mem_data)
  val tmask = Wire(Vec(inner.tlDataBeats, UInt(width=tgHelper.tagSize(inner.tlDataBits))))
  val tdata = Wire(Vec(inner.tlDataBeats, UInt(width=tgHelper.tagSize(inner.tlDataBits))))
  (0 until inner.tlDataBeats).foreach( i => {
    tmask(i) := UInt(0)
    tdata(i) := UInt(0)
  })

  val iacq_cnt = inner.acquire.bits.addr_beat
  val iacq_cnt_reg = RegEnable(iacq_cnt, inner.acquire.fire())
  val iacq_done = connectIncomingDataBeatCounter(inner.acquire)
  val ognt_cnt = outer.grant.bits.addr_beat
  val ognt_cnt_reg = RegEnable(ognt_cnt, outer.grant.fire())
  val ognt_done = connectIncomingDataBeatCounter(outer.grant)
  val (ignt_cnt, ignt_done) = connectOutgoingDataBeatCounter(inner.grant, ognt_cnt_reg)
  val (oacq_cnt, oacq_done) = connectOutgoingDataBeatCounter(outer.acquire, iacq_cnt_reg)

  def tmaskFill(tmask:UInt):UInt = {
    Vec(tmask.toBools.map(t => Mux(t, ~UInt(0, tgBits), UInt(0, tgBits)))).toBits
  }

  // inner acquire
  when(mt_state === ms_IDLE && tc_state === ms_IDLE) {
    (0 until refillCycles).foreach( i => {
      xact.wmask_buffer(i) := UInt(0)
      xact.tmask_buffer(i) := UInt(0)
    })
    tc_xact_mem_data := UInt(0)
  }

  when(inner.acquire.fire()) {
    xact.data_buffer(iacq_cnt) := inner.acquire.bits.data
    xact.wmask_buffer(iacq_cnt) := inner.acquire.bits.wmask()
    tmask(iacq_cnt) := tmaskFill(inner.acquire.bits.tmask())
    tdata(iacq_cnt) := inner.acquire.bits.tag
    write_tc_xact_data(tdata.toBits, tmask.toBits)
    xact.tmask_buffer(iacq_cnt) := inner.acquire.bits.tmask()
  }
  inner.acquire.ready := (mt_state === ms_IDLE && tc_state === ts_IDLE) || mt_state === ms_IACQ

  // outer acquire
  outer.acquire.valid := mt_state === ms_OACQ
  outer.acquire.bits := Acquire(
    is_builtin_type = Bool(true),
    a_type = Mux(xact.isBuiltInType(), xact.a_type, Acquire.getBlockType),
    client_xact_id = UInt(id),
    addr_block = xact.addr_block,
    addr_beat = oacq_cnt,
    data = xact.data_buffer(oacq_cnt),
    tag = xact_tag_buffer(oacq_cnt),
    union = Mux(xact.isBuiltInType(),
      Acquire.makeUnion( // re-assemble union to rectify wmask
        a_type = xact.a_type,
        addr_byte = xact.addr_byte(),
        operand_size = xact.op_size(),
        opcode = xact.op_code(),
        wmask = xact.wmask_buffer(oacq_cnt),
        tmask = xact.tmask_buffer(oacq_cnt),
        alloc = Bool(true)
      ),
      Cat(MT_Q, M_XRD, Bool(true))) // coherent Acquire must be getBlock?
  )(outerPM)

  // outer grant
  outer.grant.ready := mt_state === ms_OGNT
  when(outer.grant.fire()) {
    xact.data_buffer(ognt_cnt) := outer.grant.bits.data
  }

  // inner grant
  inner.grant.valid := mt_state === ms_IGNT && (!inner.grant.bits.hasData() || tc_state === ts_IDLE)
  inner.grant.bits := coh.makeGrant(
    acq = xact,
    manager_xact_id = UInt(id),
    addr_beat = ignt_cnt,
    data = xact.data_buffer(ignt_cnt),
    tag = xact_tag_buffer(ignt_cnt)
  )

  // inner finish
  inner.finish.ready := mt_state === ms_IFIN

  // tag
  tc_req_valid := iacq_done
  tc_xact_rw := xact.hasData()
  tc_xact_mem_mask := tmaskFill(xact.tmask_buffer.toBits)
  tc_xact_mem_addr := xact.full_addr()

  // tl conflicts
  io.tl_block := mt_state =/= ts_IDLE || tc_state =/= ts_IDLE
  io.tl_addr_block := xact.addr_block

  // ------------ memory tracker state machine ---------------- //
  when(mt_state === ms_IDLE && inner.acquire.fire()) {
    xact := inner.acquire.bits
    mt_state := Mux(iacq_done, ms_OACQ, ms_IACQ)
  }
  when(mt_state === ms_IACQ && iacq_done) {
    mt_state := ms_OACQ
  }
  when(mt_state === ms_OACQ && oacq_done) {
    mt_state := ms_OGNT
  }
  when(mt_state === ms_OGNT && ognt_done) {
    mt_state := ms_IGNT
  }
  when(mt_state === ms_IGNT && ignt_done) {
    if(uncached) mt_state := ms_IDLE
    else mt_state := Mux(inner.grant.bits.requiresAck(), ms_IFIN, ms_IDLE)
  }
  when(mt_state === ms_IFIN && inner.finish.valid) {
    mt_state := ms_IDLE
  }
}

// initialize the top map after power up
class TCInitiator(id:Int)(implicit p: Parameters) extends TCModule()(p) {
  val io = new Bundle {
    val mem_xact = new TCTagXactIO().flip // tag transaction from memTrackers
    val tag_xact = new TCTagXactIO        // tag transaction to tagTrackers
  }

  require(isPow2(nTagTransactors))
  val nBlocks = if(nTopMapBlocks < nTagTransactors && id < nTopMapBlocks) 1 else nTopMapBlocks / nTagTransactors

  val rst_cnt = Reg(init = UInt(0, log2Up(nBlocks+1)))
  when(rst_cnt =/= UInt(nBlocks) && io.tag_xact.req.fire()) { rst_cnt := rst_cnt + UInt(1) }

  // normal operation
  io.tag_xact.req.valid  := io.mem_xact.req.valid
  io.tag_xact.req.bits   := io.mem_xact.req.bits
  io.mem_xact.req.ready  := io.tag_xact.req.ready
  io.mem_xact.resp.valid := io.tag_xact.resp.valid
  io.mem_xact.resp.bits  := io.tag_xact.resp.bits

  // tagcache initialization for the top map
  when(rst_cnt =/= UInt(nBlocks)) {
    io.tag_xact.req.valid  := Bool(true)
    io.tag_xact.req.bits.addr := UInt(tgHelper.map1Base) + UInt(id) * UInt(p(CacheBlockBytes)) + rst_cnt * UInt(nTagTransactors * p(CacheBlockBytes))
    io.tag_xact.req.bits.op   := TCTagOp.C
    io.mem_xact.resp.valid := Bool(false)
  }

}

class TCTagXactDemux(banks: Int)(implicit p: Parameters) extends TCModule()(p) {
  val io = new Bundle {
    val in = new TCTagXactIO().flip
    val out = Vec(banks, new TCTagXactIO)
  }

  require(isPow2(banks)) // tagXactTracker is banked

  val idx = io.in.req.bits.addr >> blockOffBits
  val i_sel = Wire(Vec(banks,Bool()))
  if(banks == 1) {
    i_sel(0) := Bool(true)
  } else {
    val index = idx(log2Up(banks)-1,0)
    i_sel := i_sel.fromBits(UIntToOH(index,banks))
  }
  val o_sel = io.out.map(_.resp.valid)

  io.out.zip(i_sel).foreach{ case(o, s) => {
    o.req.valid := io.in.req.valid && s
    o.req.bits := io.in.req.bits
  }}

  io.in.req.ready := io.out.zip(i_sel).map{case(r,s)=> r.req.ready&&s}.reduce(_||_)
  io.in.resp := Mux1H(o_sel, io.out.map(_.resp))
}

////////////////////////////////////////////////////////////////
// Top level of the Tag Cache

class TagCache(implicit p: Parameters) extends TCModule()(p)
  with HasCoherenceAgentWiringHelpers
{
  val io = new ManagerTLIO

  val meta      = Module(new TCMetadataArray)
  val data      = Module(new TCDataArray)

  val relTrackers =
    (0                      until nMemReleaseTransactors).map(id => Module(new TCMemReleaseTracker(id)))
  val acqTrackers =
    (nMemReleaseTransactors until nMemAcquireTransactors).map(id => Module(new TCMemAcquireTracker(id)))
  val memTrackers = relTrackers ++ acqTrackers

  val acq_size = acqTrackers.size

  // banked tag trackers
  val tagTrackerInitiators = (0 until nTagTransactors).map( id =>
    Module(new TCInitiator(id)))

  val tagTrackers = (nMemTransactors until nMemTransactors + nTagTransactors).map(id =>
    Module(new TCTagXactTracker(id)))

  val wb = Module(new TCWritebackUnit(nMemTransactors + nTagTransactors))

  // transaction locks
  val nLocks = min(nMemTransactors, nTopMapBlocks) + 1
  val lock_vec = Reg(init = Vec.fill(nLocks)(TCTagLock()))
  val lock_avail = lock_vec.map(!_.lock)
  val lock_avail_bit = lock_avail.reduce(_||_)
  val lock_alloc = PriorityEncoder(lock_avail)
  val lock_req = tagTrackers.map(t => t.io.lock.valid && t.io.lock.bits.lock)
  val lock_req_bit = lock_req.reduce(_||_)
  val lock_req_chosen = PriorityEncoderOH(Vec(lock_req).toBits)

  val tc_req_unblock = memTrackers.map( mt => {
    val lock_addr_match = lock_vec.map(_.addr === mt.io.tc.req.bits.addr(log2Up(tgHelper.map0Size)-1, tgHelper.blockOffBits))
    val lock_id_match   = lock_vec.map(_.id === mt.io.tc.req.bits.id)
    val lock_lock       = lock_vec.map(_.lock)
    val need_lock       = tgHelper.is_map(mt.io.tc.req.bits.addr)
    val locked          = lock_addr_match.zip(lock_id_match).zip(lock_lock).map{ case ((am, im), l) => am && !im && l}.reduce(_||_)
    !need_lock || !locked
  })

  lock_vec.zipWithIndex.foreach{ case(lock, i) => {
    val lock_match = tagTrackers.map( t => t.io.lock.valid && t.io.lock.bits.id === lock.id && !t.io.lock.bits.lock &&
                                           t.io.lock.bits.addr === lock.addr)
    when(lock.lock) {
      lock.lock := Mux(lock_match.reduce(_||_), Bool(false), Bool(true))
    }.elsewhen(lock_alloc === UInt(i) && lock_req_bit) {
      lock.lock := Bool(true)
      lock.id := Mux1H(lock_req_chosen, tagTrackers.map(_.io.lock.bits.id))
      lock.addr := Mux1H(lock_req_chosen, tagTrackers.map(_.io.lock.bits.addr))
    }
  }}

  tagTrackers.zipWithIndex.foreach{ case(t, i) => {
    t.io.lock.ready := !t.io.lock.bits.lock || lock_req_chosen(i) && lock_avail_bit
  }}

  assert(memTrackers.map(_.io.tl_block).reduce(_||_) || !lock_vec.map(_.lock).reduce(_||_),
    "all tag cache lines should be unlocked when the cache is idle!")

  // connect TileLink outputs
  val outer_arb = Module(new ClientUncachedTileLinkIOArbiter(nMemTransactors + nTagTransactors + 1)
                    (p.alterPartial({ case TLId => p(OuterTLId) })))
  outer_arb.io.in <> memTrackers.map(_.outer) ++ tagTrackers.map(_.io.tl) :+ wb.io.tl
  io.outer <> outer_arb.io.out

  // connect TileLink inputs
  val acq_rel_conflict =  if(uncached) Bool(false) else {
    // when acq and rel for the same block, rel first
    io.inner.acquire.valid &&
    io.inner.release.valid &&
    io.inner.acquire.bits.addr_block === io.inner.release.bits.addr_block
  }

  val tl_acq_block = Vec(memTrackers.map(t=>{t.io.tl_block && t.io.tl_addr_block === io.inner.acquire.bits.addr_block})).toBits.orR
  val tl_acq_match = Vec(acqTrackers.map(t=>{t.io.tl_block && t.inner.acquire.ready && t.io.tl_addr_block === io.inner.acquire.bits.addr_block})).toBits
  val tl_acq_alloc = Mux(tl_acq_match.orR, tl_acq_match, Mux(tl_acq_block, UInt(0,nMemAcquireTransactors),
    PriorityEncoderOH(Vec(acqTrackers.map(_.inner.acquire.ready)).toBits)))

  acqTrackers.zipWithIndex.foreach{ case(t,i) => {
    t.inner.acquire.valid := io.inner.acquire.valid && tl_acq_alloc(i) && !acq_rel_conflict
    t.inner.acquire.bits := io.inner.acquire.bits
  }}
  io.inner.acquire.ready := tl_acq_alloc.orR

  if(!uncached) {
    val tl_rel_block = Vec(memTrackers.map(t=>{t.io.tl_block && t.io.tl_addr_block === io.inner.release.bits.addr_block})).toBits.orR
    val tl_rel_match = Vec(relTrackers.map(t=>{t.io.tl_block && t.inner.release.ready && t.io.tl_addr_block === io.inner.release.bits.addr_block})).toBits
    val tl_rel_alloc = Mux(tl_rel_match.orR, tl_rel_match, Mux(tl_rel_block, UInt(0,nMemReleaseTransactors),
      PriorityEncoderOH(Vec(relTrackers.map(_.inner.release.ready)).toBits)))

    relTrackers.zipWithIndex.foreach{ case(t,i) => {
      t.inner.release.valid := io.inner.release.valid && tl_rel_alloc(i)
      t.inner.release.bits := io.inner.release.bits
    }}
    io.inner.release.ready := tl_rel_alloc.orR
  }

  doOutputArbitration(io.inner.grant, memTrackers.map(_.inner.grant))
  doInputRouting(io.inner.finish, memTrackers.map(_.inner.finish))

  // helpers for internal connection
  def doTcOutputArbitration[T <: Bundle](out: DecoupledIO[T], ins: Seq[DecoupledIO[T]])
  {
    val arb = Module(new RRArbiter(out.bits, ins.size))
    out <> arb.io.out
    arb.io.in <> ins
  }

  def doTcInputRouting[T <: Bundle with HasTCId](in: ValidIO[T], outs: Seq[ValidIO[T]], base:Int = 0) {
    outs.zipWithIndex.foreach{ case(out, i) => {
      out.bits := in.bits
      out.valid := in.valid && in.bits.id === UInt(base+i)
    }}
  }

  // connect transactions from memTrackers to TagTrackers
  val tagXactDemuxers = memTrackers.map(_ => Module(new TCTagXactDemux(nTagTransactors)))
  memTrackers.zip(tagXactDemuxers).zip(tc_req_unblock)foreach{ case((t, m), b) => {
    m.io.in.req.valid := t.io.tc.req.valid && b
    m.io.in.req.bits  := t.io.tc.req.bits
    t.io.tc.req.ready := m.io.in.req.ready && b
    t.io.tc.resp <> m.io.in.resp
  }}

  tagTrackers.zip(tagTrackerInitiators).zipWithIndex.foreach{ case((t, ti), i) => {
    t.io.xact <> ti.io.tag_xact
    doTcOutputArbitration(ti.io.mem_xact.req, tagXactDemuxers.map(_.io.out(i).req))
    doTcInputRouting(ti.io.mem_xact.resp, tagXactDemuxers.map(_.io.out(i).resp))
  }}

  // connect tagXactTrackers to meta array
  doTcOutputArbitration(meta.io.read, tagTrackers.map(_.io.meta.read))
  doTcOutputArbitration(meta.io.write, tagTrackers.map(_.io.meta.write))
  doTcInputRouting(meta.io.resp, tagTrackers.map(_.io.meta.resp), nMemTransactors)

  // connect tagXactTrackers and writeback unit to data array
  doTcOutputArbitration(data.io.read, tagTrackers.map(_.io.data.read) :+ wb.io.data.read)
  doTcOutputArbitration(data.io.write, tagTrackers.map(_.io.data.write) :+ wb.io.data.write)
  doTcInputRouting(data.io.resp, tagTrackers.map(_.io.data.resp) :+ wb.io.data.resp, nMemTransactors)

  // connect tagXactTrackers to writeback unit
  doTcOutputArbitration(wb.io.xact.req, tagTrackers.map(_.io.wb.req))
  doTcInputRouting(wb.io.xact.resp, tagTrackers.map(_.io.wb.resp), nMemTransactors)

}


class TagCacheTop(param: Parameters) extends Module
{

  implicit val p = param.alterPartial({
    case CacheName => "TagCache"
    case TLId => "TCtoMem"
    case InnerTLId => "L2toTC"
    case OuterTLId => "TCtoMem"
    case BusId => "mem"
  })

  def connectNasti(outer: NastiIO, inner: NastiIO)(implicit p: Parameters) {
    outer.ar <> Queue(inner.ar,1)
    outer.aw <> Queue(inner.aw,1)
    outer.w  <> Queue(inner.w,1)
    inner.r  <> Queue(outer.r,1)
    inner.b  <> Queue(outer.b,1)
  }

  // connect uncached tilelike -> nasti
  def connectTilelinkNasti(nasti: NastiIO, tl: ClientUncachedTileLinkIO)(implicit p: Parameters) = {
    val conv = Module(new NastiIOTileLinkIOConverter())
    conv.io.tl <> tl
    connectNasti(nasti, conv.io.nasti)
  }

  val io = new Bundle {
    val in = new ManagerTileLinkIO()(p.alterPartial({case TLId => "L2toTC"}))
    val out = new NastiIO()
  }

  val tc = Module(new TagCache)
  tc.io.inner <> io.in
  tc.io.incoherent := Vec.fill(tc.io.inner.tlNCachingClients)(Bool(false))
  connectTilelinkNasti(io.out, tc.io.outer)
}
