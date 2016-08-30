// See LICENSE for license details.

package uncore
import Chisel._
import cde.{Parameters, Field}
import junctions._

case object TCMemTransactors extends Field[Int]
case object TCTagTransactors extends Field[Int]
case object TCEarlyAck extends Field[Boolean]

trait HasTCParameters extends HasCoherenceAgentParameters
    with HasCacheParameters with HasTagParameters
{
  val nMemAcquireTransactors = p(TCMemTransactors)
  val nMemReleaseTransactors = 1
  val nMemTransactors = nMemReleaseTransactors + nMemAcquireTransactors
  val nTagTransactors = p(TCTagTransactors)

  val refillCyclesPerBeat = outerDataBits/rowBits
  val refillCycles = refillCyclesPerBeat*outerDataBeats

  val cacheAccDelay = 2 // delay of accessing metadata or data arrays

  val tcTagTLParams = p.alterPartial({
    case CacheName => "TagCache"
    case InnerTLId => "TC"
  })

  val earlyAck = p(TCEarlyAck) // early ack optimization to reduce delay

  val rowTags = rowBits / tgHelper.wordBits
  val rowTagBits = tgBits * rowTags

  require(p(CacheBlockBytes) * tgBits / 8 <= rowBits) // limit the data size of tag operation to a row
  require(!outerTLParams.withTag && innerTLParams.withTag)
  require(outerTLId == p(TLId))
}

abstract class TCModule(implicit val p: Parameters) extends Module with HasTCParameters
abstract class TCBundle(implicit val p: Parameters) extends ParameterizedBundle()(p) with HasTCParameters

trait HasTCMemXactId extends HasTCParameters { val id = UInt(width = log2Up(nMemTransactors)) }
trait HasTCTagXactId extends HasTCParameters { val id = UInt(width = log2Up(nTagTransactors)) }
trait HasTCTagFlag   extends HasTCParameters { val tagFlag = UInt(width = 1) }
trait HasTCTag       extends HasTCParameters { val tag = UInt(width = tagBits) }
trait HasTCHit       extends HasTCParameters { val hit = Bool() }

class TCMIIncoherence(implicit p: Parameters) extends CoherenceMetadata()(p) {
  val state = Bool()

  def isValid(dummy:Int=0):Bool = state
}

object TCMIIncoherence {
  def onReset(implicit p: Parameters) = {
    val meta = Wire(new TCMIIncoherence)
    meta.state := Bool(false)
    meta
  }
  def onGrant(implicit p: Parameters) = {
    val meta = Wire(new TCMIIncoherence)
    meta.state := Bool(true)
    meta
  }
}

class TCMetadata(implicit p: Parameters) extends Metadata()(p) with HasTCParameters
{
  val coh = new TCMIIncoherence
  val tagFlag = UInt(width = 1)
  override def cloneType = new TCMetadata().asInstanceOf[this.type]
}

object TCMetadata {
  def apply(tag:UInt, tagFlag:UInt, coh: TCMIIncoherence)(implicit p: Parameters) = {
    val meta = Wire(new TCMetadata)
    meta.tag := tag
    meta.tagFlag := tagFlag
    meta.coh := coh
    meta
  }
}

class TCMetaReadReq(implicit p: Parameters) extends MetaReadReq()(p) with HasTCTag with HasTCTagXactId
class TCMetaWriteReq(implicit p: Parameters) extends MetaWriteReq[TCMetadata](new TCMetadata)(p)
class TCMetaReadResp(implicit p: Parameters) extends TCBundle()(p) with HasTCTagXactId with HasTCHit {
  val meta = new TCMetadata
  val way_en = UInt(width = nWays)
}

class TCMetaIO(implicit p: Parameters) extends TCBundle()(p) {
  val read = Decoupled(new TCMetaReadReq)
  val write = Decoupled(new TCMetaWriteReq)
  val resp = Valid(new TCMetaReadResp).flip
  override def cloneType = new TCMetaIO().asInstanceOf[this.type]
}

trait HasTCRow extends HasTCParameters  { val row  = UInt(width = log2Up(refillCycles)) }
trait HasTCData extends HasTCParameters { val data = UInt(width = rowBits) }
trait HasTCMask extends HasTCParameters { val mask = UInt(width = rowBytes) }
trait HasTCAddr extends HasTCParameters { val addr = UInt(width=p(PAddrBits)) }

class TCDataReadReq(implicit p: Parameters) extends TCBundle()(p) with HasTCTagXactId with HasTCRow {
  val idx = Bits(width = idxBits)
  val way_en = Bits(width = nWays)
}
class TCDataWriteReq(implicit p: Parameters) extends TCDataReadReq()(p) with HasTCData with HasTCMask
class TCDataReadResp(implicit p: Parameters) extends TCBundle()(p)
    with HasTCTagXactId with HasTCData

class TCDataIO(implicit p: Parameters) extends TCBundle()(p) {
  val read = Decoupled(new TCDataReadReq)
  val write = Decoupled(new TCDataWriteReq)
  val resp = Valid(new TCDataReadResp).flip
  override def cloneType = new TCDataIO().asInstanceOf[this.type]
}

class TCWBReq(implicit p: Parameters) extends TCDataReadReq()(p) with HasTCTag
class TCWBResp(implicit p: Parameters) extends TCBundle()(p) with HasTCTagXactId

class TCWBIO(implicit p: Parameters) extends TCBundle()(p) {
  val req = Decoupled(new TCWBReq)
  val resp = Valid(new TCWBResp).flip
}

object TCTagOp {
  def nBits           = 3
  def Read            = UInt("b001")
  def FetchRead       = UInt("b101")
  def Write           = UInt("b010")
  def Create          = UInt("b110")
  def isRead(t:UInt)  = t(0)
  def isWrite(t:UInt) = t(1)
}

class TCTagRequest(implicit p: Parameters) extends TCBundle()(p)
    with HasTCMemXactId with HasTCData with HasTCMask with HasTCAddr
{
  val op   = UInt(width=TCTagOp.nBits)
}

class TCTagResp(implicit p: Parameters) extends TCBundle()(p) with HasTCMemXactId
    with HasTCHit with HasTCData with HasTCTagFlag

class TCTagXactIO(implicit p: Parameters) extends TCBundle()(p) {
  val req = Decoupled(new TCTagRequest)
  val resp = Valid(new TCTagResp).flip
}

////////////////////////////////////////////////////////////////
// tag cache metadata array

class TCMetadataArray(implicit p: Parameters) extends TCModule()(p) {
  val io = new TCMetaIO().flip

  val replacer = new RandomReplacement(nWays)
  val ren = io.read.fire()
  val onReset = () => TCMetadata(UInt(0), UInt(0), TCMIIncoherence.onReset)
  val meta = Module(new MetadataArray[TCMetadata](onReset))
  meta.io.read <> io.read
  meta.io.read.bits.way_en := (Vec.fill(nWays){Bool(true)}).toBits
  meta.io.write <> io.write

  val s1_read_valid = Reg(next = ren)
  val s1_id         = RegEnable(io.read.bits.id, ren)
  val s1_tag        = RegEnable(io.read.bits.tag, ren)
  val s1_match_way  = Vec(meta.io.resp.map(m => m.tag === s1_tag && m.coh.isValid())).toBits

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
  io.resp.bits.meta.tag := UInt(0)
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
  }

  val s_IDLE :: s_READ :: s_READ_DONE :: s_WRITE :: Nil = Enum(UInt(), 4)
  val state = Reg(init = s_IDLE)

  val data_buffer = Reg(init=Vec.fill(refillCycles)(UInt(0, rowBits)))
  val (read_cnt, read_done) = Counter(io.data.read.fire() && state === s_READ, refillCycles)
  val (write_cnt, write_done) = Counter(io.data.resp.valid, refillCycles)

  val tl_buffer = Wire(Vec(outerDataBeats, UInt(width=outerDataBits)))
  tl_buffer := tl_buffer.fromBits(data_buffer.toBits)
  val (tl_cnt, tl_done) = Counter(state === s_WRITE && io.tl.acquire.fire(), outerDataBeats)

  val xact = RegEnable(io.xact.req.bits, io.xact.req.fire())
  io.xact.req.ready := state === s_IDLE

  io.xact.resp.bits.id := xact.id
  if (earlyAck) {
    io.xact.resp.valid := state === s_READ && read_done
  } else {
    io.xact.resp.valid := state === s_WRITE && tl_done
  }

  io.data.read.valid := state === s_READ
  io.data.read.bits := xact
  io.data.read.bits.id := UInt(id)
  io.data.read.bits.row := read_cnt

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
      tl_buffer(tl_cnt)
    )

  when(state === s_IDLE && io.xact.req.valid) {
    state := s_READ
  }
  when(state === s_READ && read_done) {
    // check whether data is available in time considering the accessing delay
    if(outerDataBeats >= cacheAccDelay) {
      state := s_WRITE
    } else {
      state := s_READ_DONE
    }
  }
  when(state === s_READ_DONE && write_done) {
    state := s_WRITE
  }
  when(io.xact.resp.valid) {
    state := s_IDLE
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
  }

  // meta.R             always read metadata first
  // data.FWB           fetch and write back, fatch when F+R/W.miss
  //                    WB when F+R/W.miss and C needs replace write-back
  // data.R             read data array when W.h and F+R/R.h.t=1
  // data.BW            write a whole cache block in data array when F+R/W.m and C
  // data.RW            write a row in data array when W.h
  // data.C             check the whole cache line to see if potentially tagFlag reset
  // meta.W             write metadata when F.m, C, W.m and W.h.t!=t'

  val s_IDLE :: s_M_R_REQ :: s_M_R_RESP :: s_D_FWB_REQ :: s_D_FWB_RESP :: s_D_R_REQ :: s_D_R_RESP :: s_D_BW :: s_D_RW :: s_D_C_REQ :: s_D_C_RESP :: s_M_W :: Nil = Enum(UInt(), 12)
  val state = Reg(init = s_IDLE)

  // internal signals
  val xact = RegEnable(io.xact.req.bits, io.xact.req.fire())
  val xact_resp_done = Reg(init=Bool(false)) // simplify early ack check (no double ack)
  val idx = xact.addr(idxBits+blockOffBits-1, blockOffBits)
  val way_en = Reg(io.meta.resp.bits.way_en, state === s_M_R_RESP && io.meta.resp.valid)
  val row = xact.addr(blockOffBits-1,rowOffBits)
  val meta = Reg(new TCMetadata)
  val addrTag = xact.addr >> untagBits
  val write_buf = Wire(Vec(refillCycles, UInt(width=rowBits)))
  val (data_cnt, data_done) = Counter((state === s_D_BW && io.data.write.fire()) ||
                                      (state === s_D_C_REQ && io.data.read.fire()), refillCycles)
  val write_tag = Vec((0 until rowBytes).map(i => {
      xact.mask(i) && xact.data(i*8+7,i*8).orR
    })).toBits.orR
  val writeback_sent = Reg(init=Bool(false))
  val writeback_done = Reg(init=Bool(false))
  val fetch_sent = Reg(init=Bool(false))
  val fetch_done = Reg(init=Bool(false))
  val fetch_buf = Reg(Vec(outerDataBeats, UInt(width=outerDataBits)))
  val (fetch_cnt, fetch_cnt_done) = Counter(state === s_D_FWB_RESP && io.tl.grant.fire(), outerDataBeats)
  val (_, check_done) = Counter((state === s_D_C_REQ || state === s_D_C_RESP) &&
                                io.data.read.fire(), refillCycles)

  // transaction request
  io.xact.req.ready := state === s_IDLE

  // transaction response
  io.xact.resp.bits.id := xact.id
  io.xact.resp.bits.hit := Bool(true)
  io.xact.resp.bits.tagFlag := meta.tagFlag
  io.xact.resp.bits.data := UInt(0)
  io.xact.resp.valid := Bool(false)

  // metadata read
  io.meta.read.bits.id := UInt(id)
  io.meta.read.bits.idx := idx
  io.meta.read.bits.tag := addrTag
  io.meta.read.valid := state === s_M_R_REQ

  // metadata write
  io.meta.write.bits.idx := idx
  io.meta.write.bits.way_en := way_en
  io.meta.write.bits.data := meta
  io.meta.write.valid := state === s_M_W

  // data array read
  io.data.read.bits.id := UInt(id)
  io.data.read.bits.row := row
  io.data.read.bits.idx := idx
  io.data.read.bits.way_en := way_en
  io.data.read.valid := state === s_D_R_REQ

  // data read response

  // data array write
  io.data.write.bits.id := UInt(id)
  io.data.write.bits.row := Mux(state === s_D_RW, row, data_cnt)
  io.data.write.bits.idx := idx
  io.data.write.bits.way_en := way_en
  io.data.write.bits.data := Mux(state === s_D_BW, write_buf(data_cnt), xact.data)
  io.data.write.bits.mask := Mux(state === s_D_BW, (Vec.fill(rowBits){Bool(true)}).toBits, xact.mask)
  io.data.write.valid := state === s_D_RW || state === s_D_BW
  write_buf := write_buf.fromBits(fetch_buf.toBits)

  // write-back
  io.wb.req.bits.id := UInt(id)
  io.wb.req.bits.row := row
  io.wb.req.bits.idx := idx
  io.wb.req.bits.way_en := way_en
  io.wb.req.bits.tag := meta.tag
  io.wb.req.valid := state === s_D_FWB_RESP && !writeback_sent

  when(state === s_D_FWB_REQ) {
    when(!meta.coh.isValid() || meta.tagFlag === UInt(0)) { // also need to check safe condition
      writeback_sent := Bool(true)
      writeback_done := Bool(true)
    }
  }
  when(state === s_D_FWB_RESP) {
    when(io.wb.req.fire()) {
      writeback_sent := Bool(true)
    }
    when(io.wb.resp.valid) {
      writeback_sent := Bool(true)
    }
    when(writeback_done && fetch_done) {
      writeback_sent := Bool(false)
      writeback_done := Bool(false)
    }
  }

  // fetch
  io.tl.acquire.bits := GetBlock(UInt(id), tlGetBlockAddr(xact.addr))
  io.tl.acquire.valid := state === s_D_FWB_RESP && !fetch_sent
  io.tl.grant.ready := state === s_D_FWB_RESP && !fetch_done

  // meta check
  when((state === s_D_C_REQ || state === s_D_C_RESP) && io.data.read.fire()) {
    meta.tagFlag := meta.tagFlag =/= UInt(0) || io.data.resp.bits.data =/= UInt(0)
  }

  // data array update function
  def beat_data_update(tl_data:UInt, index:UInt) = {
    val update_mask = Wire(UInt(xact.mask << tlGetByteAddr(xact.addr), tlDataBytes))
    val write_data = Fill(refillCyclesPerBeat, xact.data)
    Mux(index === tlGetBeatAddr(xact.addr), tl_data,
      Vec((0 until outerDataBits/8).map(i => {
        Mux(update_mask(i), write_data(i*8+7,i*8), tl_data(i*8+7,i*8))
      })).toBits)
  }

  when(state === s_D_FWB_REQ) {
    meta := TCMetadata(addrTag, UInt(0), TCMIIncoherence.onReset)
    when(xact.op === TCTagOp.Create) {
      (0 until outerDataBeats).foreach(i => {
        fetch_buf(i) := beat_data_update(UInt(0,outerDataBits), UInt(i))
      })
      meta.tagFlag := write_tag
      fetch_sent := Bool(true)
      fetch_done := Bool(true)
    }
  }
  when(state === s_D_FWB_RESP) {
    when(io.tl.acquire.fire()) {
      fetch_sent := Bool(true)
    }
    when(io.tl.grant.valid) {
      val m_update_data = beat_data_update(io.tl.grant.bits.data, fetch_cnt)
      fetch_buf(fetch_cnt) := m_update_data
      meta.tagFlag := meta.tagFlag =/= UInt(0) || m_update_data =/= UInt(0)
      fetch_done := fetch_cnt_done
    }
    when(writeback_done && fetch_done) {
      meta.tag := addrTag
      meta.coh := TCMIIncoherence.onGrant
      fetch_sent := Bool(false)
      fetch_done := Bool(false)
    }
  }

  // state machine
  when(state === s_IDLE) {
    if(earlyAck) xact_resp_done := Bool(false)
    state := s_M_R_REQ
  }
  when(state === s_M_R_REQ && io.meta.read.fire()) {
    state := s_M_R_RESP
  }
  when(state === s_M_R_RESP && io.meta.resp.valid) {
    meta := io.meta.resp.bits.meta
    when(io.meta.resp.bits.hit) {
      when(io.meta.resp.bits.meta.tagFlag === UInt(0)) {
        when(TCTagOp.isRead(xact.op)) {
          // R/F+R hit,t==0
          io.xact.resp.bits.tagFlag := UInt(0)
          io.xact.resp.valid := Bool(true)
          state := s_IDLE
        }.otherwise{
          // W/C hit,t==0
          if(earlyAck) {
            io.xact.resp.bits.tagFlag := write_tag
            xact_resp_done := Bool(true)
            io.xact.resp.valid := Bool(true)
          }
          meta.tagFlag := write_tag
          xact.op := TCTagOp.Create // Use Create to denote t==0
          state := s_D_RW
        }
      }.otherwise{
        // All hit,t!=0
        state := s_D_R_REQ
      }
    }.otherwise{
      when(xact.op === TCTagOp.Read) {
        // R miss
        io.xact.resp.bits.hit := Bool(false)
        io.xact.resp.valid := Bool(true)
        state := s_IDLE
      }.otherwise{
        // F+R/W/C miss
        state := s_D_FWB_REQ
      }
    }
  }
  when(state === s_D_FWB_REQ) {
    state := s_D_FWB_RESP
  }
  when(state === s_D_FWB_RESP && fetch_done && writeback_done) {
    // F+R/W/C miss
    if(earlyAck) {
      xact_resp_done := Bool(true)
      io.xact.resp.bits.data := write_buf(row)
      io.xact.resp.valid := Bool(true)
    }
    when(TCTagOp.isRead(xact.op)) {
      xact.data := write_buf(row) // store read data for final xact.resp
    }
    state := s_D_BW
  }
  when(state === s_D_R_REQ && io.data.read.fire()) {
    state := s_D_R_RESP
  }
  when(state === s_D_R_RESP && io.data.resp.valid) {
    when(TCTagOp.isRead(xact.op)) {
      // R/F+R hit,t!=0
      io.xact.resp.bits.data := io.data.resp.bits.data
      io.xact.resp.valid := Bool(true)
      state := s_IDLE
    }.otherwise{
      // W/C hit, t!=0
      when(write_tag) { // if t' != 0
        if(earlyAck) {
          xact_resp_done := Bool(true)
          io.xact.resp.valid := Bool(true)
        }
      }
      xact.op := TCTagOp.Write
      meta.tagFlag := write_tag
      state := s_D_RW
    }
  }
  when(state === s_D_BW && data_done) {
    state := s_M_W
  }
  when(state === s_D_RW && io.data.write.fire()) {
    when((xact.op === TCTagOp.Create && meta.tagFlag === UInt(0)) ||
         (xact.op === TCTagOp.Write  && meta.tagFlag =/= UInt(0)))
    {
      // t == t'
      io.xact.resp.valid := !xact_resp_done
      state := s_IDLE
    }.elsewhen(xact.op === TCTagOp.Create && meta.tagFlag =/= UInt(0)) {
      // t: 0->1
      state := s_M_W
    }.otherwise{
      // t: 1->0?
      meta.tagFlag := UInt(0)
      state := s_D_C_REQ
    }
  }
  when(state === s_D_C_REQ && data_done) {
    state := s_D_C_RESP
  }
  when(state === s_D_C_RESP && check_done) {
    when((meta.tagFlag =/= UInt(0) || io.data.resp.bits.data =/= UInt(0))) {
      state := s_IDLE
    }.otherwise{
      // really t:1->0
      if(earlyAck) {
        xact_resp_done := Bool(true)
        io.xact.resp.valid := Bool(true)
      }
      state := s_M_W
    }
  }
  when(state === s_M_W && io.meta.write.fire()) {
    io.xact.resp.bits.data := xact.data // for F+R
    io.xact.resp.valid := !xact_resp_done
    state := s_IDLE
  }

}

////////////////////////////////////////////////////////////////
// Memory Transaction Tracker

class TCXact(implicit p: Parameters) extends TCBundle()(p) with HasTCAddr {
  val rw = Bool() // r:0 w:1
  val mem_data = UInt(width = rowBits)
  val mem_mask = UInt(width = rowBytes)
  val tt_data = UInt(width = rowBits)
  val tt_tagFlag = Bool()
  val tm0_data = UInt(width = 8)
  val tm0_tagFlag = Bool()
  val tm1_data = UInt(width = 8)
  val tm1_tagFlag = Bool()
}

class TCMemXactTracker(id: Int)(implicit p: Parameters) extends TCModule()(p)
    with HasDataBeatCounters
{
  val io = new Bundle {
    val inner = new ManagerTileLinkIO()(p.alterPartial({case TLId => p(InnerTLId)}))
    val outer = new ClientUncachedTileLinkIO()(p.alterPartial({case TLId => p(OuterTLId)}))
    val tc = new TCTagXactIO
    val tc_write = Bool(OUTPUT) // enforce temporal write order
  }
  def inner: ManagerTileLinkIO = io.inner
  def outer: ClientUncachedTileLinkIO = io.outer
  val coh = ManagerMetadata.onReset

  // ------------ tag cache state machine states -------------- //
  // TT.R               read tag table
  // TM0.R              read tag map 0
  // TM1.F+R            force read tag map 1
  // TM0.F+R            force read tag map 0
  // TM0.C              create an empty line in tag map 0
  // TT.F+R             force read tag table
  // TT.C               create a line in tag table
  // TT.W               write to tag table
  // TM0.W              write to tag map 0
  // TM1.W              write to tag map 1

  val ts_IDLE :: ts_TT_R :: ts_TM0_R :: ts_TM1_FR :: ts_TM0_FR :: ts_TT_FR :: ts_TM0_C :: ts_TT_C :: ts_TT_W :: ts_TM0_W :: ts_TM1_W :: Nil = Enum(UInt(), 11)
  val tc_state = Reg(init = ts_IDLE)
  val tc_state_next = tc_state
  tc_state := tc_state_next
  val tc_xact = Reg(new TCXact)                 // record of tag cache search procedure
  val tc_req_valid = Wire(Bool())               // start the tag cache search transaction
  val tx_req_fire = Reg(init = Bool(false))     // tag transaction request fired
  val tx_resp_hit = Reg(init = Bool(false))     // tag cache search hit
  val tx_resp_valid = Reg(init = Bool(false))   // response from tag transaction trackers ready
  val tc_wdata_valid = Reg(init = Bool(false))  // the tag write data is ready from TileLink
  val tc_tt_update =                            // whether needs to update tag table
    (0 until rowBytes).map( i => {
      tc_xact.mem_mask(i) && tc_xact.mem_data(i*8+7,i*8) =/= tc_xact.tt_data(i*8+7,i*8)
    }).reduce(_ || _)
  val tc_tt_addr = tgHelper.pa2tta(tc_xact.addr)
  val tc_tt_byte_index = tgHelper.pa2ttr(tc_xact.addr, rowOffBits)
  val tc_tt_valid = Reg(init = Bool(false))     // identify when tc_xact.tt_data is ready
  val tc_tm0_addr = tgHelper.pa2tm0a(tc_xact.addr)
  val tc_tm0_bit_index = tgHelper.pa2tm0b(tc_xact.addr)
  val tc_tm0_byte_index = tgHelper.pa2tm0r(tc_xact.addr, rowOffBits)
  val tc_tm0_bit = tc_xact.tm0_data(tgHelper.pa2tm0b(tc_xact.addr))
  val tc_tm1_addr = tgHelper.pa2tm1a(tc_xact.addr)
  val tc_tm1_bit_index = tgHelper.pa2tm1b(tc_xact.addr)
  val tc_tm1_byte_index = tgHelper.pa2tm1r(tc_xact.addr, rowOffBits)
  val tc_tm1_bit = tc_xact.tm1_data(tgHelper.pa2tm1b(tc_xact.addr))

  when(tc_state =/= tc_state_next) {
    tx_req_fire := Bool(false)
    tx_resp_valid := Bool(false)
  }
  when(io.tc.req.fire()) {
    tx_req_fire := Bool(true)
  }
  when(io.tc.resp.valid) {
    tx_resp_hit := io.tc.resp.bits.hit
    tx_resp_valid := Bool(true)
  }

  // tag transaction request
  io.tc_write := Bool(false)
  io.tc.req.valid := !tx_req_fire && tc_state =/= ts_IDLE
  io.tc.req.bits.id   := UInt(id)
  io.tc.req.bits.data := UInt(0)
  io.tc.req.bits.mask := UInt(0)
  io.tc.req.bits.addr := UInt(0)
  io.tc.req.bits.op   := UInt(0)
  switch(tc_state) {
    is(ts_TT_R) {
      io.tc.req.bits.addr := tc_tt_addr
      io.tc.req.bits.op   := TCTagOp.Read
    }
    is(ts_TM0_R) {
      io.tc.req.bits.addr := tc_tm0_addr
      io.tc.req.bits.op   := TCTagOp.Read
    }
    is(ts_TM1_FR) {
      io.tc.req.bits.addr := tc_tm1_addr
      io.tc.req.bits.op   := TCTagOp.FetchRead
    }
    is(ts_TM0_FR) {
      io.tc.req.bits.addr := tc_tm0_addr
      io.tc.req.bits.op   := TCTagOp.FetchRead
    }
    is(ts_TT_FR) {
      io.tc.req.bits.addr := tc_tt_addr
      io.tc.req.bits.op   := TCTagOp.FetchRead
    }
    is(ts_TM0_C) {
      io.tc.req.bits.addr := tc_tm0_addr
      io.tc.req.bits.op   := TCTagOp.Create
    }
    is(ts_TT_C) {
      io.tc.req.bits.data := tc_xact.mem_data
      io.tc.req.bits.mask := tc_xact.mem_mask
      io.tc.req.bits.addr := tc_tt_addr
      io.tc.req.bits.op   := TCTagOp.Create
      io.tc_write         := Bool(true)
    }
    is(ts_TT_W) {
      io.tc.req.bits.data := tc_xact.mem_data
      io.tc.req.bits.mask := tc_xact.mem_mask
      io.tc.req.bits.addr := tc_tt_addr
      io.tc.req.bits.op   := TCTagOp.Write
      io.tc_write         := Bool(true)
    }
    is(ts_TM0_W) {
      io.tc.req.bits.data :=
        Mux(tc_xact.tt_tagFlag,
          tc_xact.tm0_data | (UInt(1,8) << tc_tm0_bit_index),
          tc_xact.tm0_data & ~(UInt(1,8) << tc_tm0_bit_index)
        ) << (tc_tm0_byte_index * UInt(8))
      io.tc.req.bits.mask := UInt(1,rowBytes) << tc_tm0_byte_index
      io.tc.req.bits.addr := tc_tm0_addr
      io.tc.req.bits.op   := TCTagOp.Write
      io.tc_write         := Bool(true)
    }
    is(ts_TM1_W) {
      io.tc.req.bits.data :=
        Mux(tc_xact.tm0_tagFlag,
          tc_xact.tm1_data | (UInt(1,8) << tc_tm1_bit_index),
          tc_xact.tm1_data & ~(UInt(1,8) << tc_tm1_bit_index)
        ) << (tc_tm1_byte_index * UInt(8))
      io.tc.req.bits.mask := UInt(1,rowBytes) << tc_tm1_byte_index
      io.tc.req.bits.addr := tc_tm1_addr
      io.tc.req.bits.op   := TCTagOp.Write
      io.tc_write         := Bool(true)
    }
  }

  // tag transaction response
  when(io.tc.resp.valid) {
    switch(tc_state) {
      is(ts_TT_R) {
        tc_xact.tt_data := io.tc.resp.bits.data
        tc_xact.tt_tagFlag := io.tc.resp.bits.tagFlag
      }
      is(ts_TM0_R) {
        tc_xact.tm0_data := io.tc.resp.bits.data >> (tc_tm0_byte_index * UInt(8))
        tc_xact.tm0_tagFlag := io.tc.resp.bits.tagFlag
      }
      is(ts_TM1_FR) {
        tc_xact.tm1_data := io.tc.resp.bits.data >> (tc_tm1_byte_index * UInt(8))
        tc_xact.tm1_tagFlag := io.tc.resp.bits.tagFlag
      }
      is(ts_TM0_FR) {
        tc_xact.tm0_data := io.tc.resp.bits.data >> (tc_tm0_byte_index * UInt(8))
        tc_xact.tm0_tagFlag := io.tc.resp.bits.tagFlag
      }
      is(ts_TT_FR) {
        tc_xact.tt_data := io.tc.resp.bits.data
        tc_xact.tt_tagFlag := io.tc.resp.bits.tagFlag
      }
      is(ts_TM0_C) {
        tc_xact.tm0_tagFlag := io.tc.resp.bits.tagFlag
      }
      is(ts_TT_C) {
        tc_xact.tt_tagFlag := io.tc.resp.bits.tagFlag
      }
      is(ts_TT_W) {
        tc_xact.tt_tagFlag := io.tc.resp.bits.tagFlag
      }
      is(ts_TM0_W) {
        tc_xact.tm0_tagFlag := io.tc.resp.bits.tagFlag
      }
    }
  }

  // -------------- the shared state machine ----------------- //
  when(tc_state === ts_IDLE && tc_req_valid) {
    tc_tt_valid := Bool(false)
    when(Bool(true)) {          // most tagged
      tc_state_next := ts_TT_R
    }.otherwise{                // most untagged
      tc_state_next := ts_TM1_FR
    }
  }
  when(tx_resp_valid) {
    when(tc_state === ts_TT_R) {
      when(tx_resp_hit) {
        tc_tt_valid := Bool(true)
        when(!tc_xact.rw) { // MEM.R TT.hit
          tc_state_next := ts_IDLE
        }.elsewhen(tc_wdata_valid) { // MEM.W TT.hit
          tc_state_next := Mux(tc_tt_update, ts_TT_W, ts_IDLE)
        }
      }.otherwise{ // TT.miss
        tc_xact.tt_data := UInt(0) // assuming TT=0
        tc_xact.tt_tagFlag := Bool(false)
        tc_state_next := ts_TM0_R
      }
    }
    when(tc_state === ts_TM0_R) {
      when(tx_resp_hit) {
        tc_tt_valid := !tc_tm0_bit
        when(tc_tm0_bit) { // TM0.hit TM0=1
          tc_state_next := ts_TT_FR
        }.elsewhen(!tc_xact.rw) { // MEM.R TM0.hit TM0=0
          tc_state_next := ts_IDLE
        }.elsewhen(tc_wdata_valid) { // MEM.W TM0.hit TM0=0
          tc_state_next := Mux(tc_tt_update, ts_TT_C, ts_IDLE)
        }
      }.otherwise{ // TM0.miss
        tc_state_next := ts_TM1_FR
      }
    }
    when(tc_state === ts_TM1_FR) {
      tc_tt_valid := !tc_tm1_bit
      when(tc_tm1_bit) { // TM1=1
        tc_state_next := ts_TM0_FR
      }.elsewhen(!tc_xact.rw) { // MEM.R TM1=0
        tc_state_next := ts_IDLE
      }.elsewhen(tc_wdata_valid) { // MEM.W TM1=0
        tc_state_next := Mux(tc_tt_update, ts_TM0_C, ts_IDLE)
      }
    }
    when(tc_state === ts_TM0_FR) {
      tc_tt_valid := !tc_tm0_bit
      when(tc_tm0_bit) { // TM0=1
        tc_state_next := ts_TT_FR
      }.elsewhen(!tc_xact.rw) { // MEM.R TM0=0
        tc_state_next := ts_IDLE
      }.elsewhen(tc_wdata_valid) { // MEM.W TM0=0
        tc_state_next := Mux(tc_tt_update, ts_TT_C, ts_IDLE)
      }
    }
    when(tc_state === ts_TM0_C) {
      tc_state_next := ts_TT_C
    }
    when(tc_state === ts_TT_FR) {
      tc_tt_valid := Bool(true)
      when(!tc_xact.rw) { // MEM.R
        tc_state_next := ts_IDLE
      }.elsewhen(tc_wdata_valid) { // MEM.W
        tc_state_next := Mux(tc_tt_update, ts_TT_W, ts_IDLE)
      }
    }
    when(tc_state === ts_TT_C) {
      tc_state_next := ts_TM0_W
    }
    when(tc_state === ts_TT_W) {
      tc_state_next := Mux(tc_tm0_bit =/= tc_xact.tt_tagFlag, ts_TM0_W, ts_IDLE)
    }
    when(tc_state === ts_TM0_W) {
      tc_state_next := Mux(tc_tm1_bit =/= tc_xact.tm0_tagFlag, ts_TM1_W,ts_IDLE)
    }
    when(tc_state === ts_TM1_W) {
      tc_state_next := ts_IDLE
    }
  }
}

class TCMemReleaseTracker(id: Int)(implicit p: Parameters) extends TCMemXactTracker(id)(p) {

  // ------------ tag cache state machine states -------------- //
  val ms_IDLE :: ms_IREL :: ms_OACQ :: ms_IGNT :: Nil = Enum(UInt(), 4)
  val mt_state = Reg(init = ms_IDLE)

  val xact = Reg(new BufferedReleaseFromSrc()(p.alterPartial({ case TLId => innerTLId })))
  val i_data = Wire(Vec(innerDataBeats, UInt(width=innerDataBits)))
  val i_tag  = Wire(Vec(innerDataBeats, UInt(width=tgHelper.sizeOfTag(innerDataBits))))
  val o_data = Wire(Vec(outerDataBeats, UInt(width=outerDataBits)))

  val irel_done = connectIncomingDataBeatCounter(inner.release)
  val (oacq_cnt, oacq_done) = connectOutgoingDataBeatCounter(outer.acquire)

  // inner release
  when(inner.release.fire()) {
    xact.data_buffer(inner.release.bits.addr_beat) := inner.release.bits.data
  }
  inner.release.ready := (mt_state === ms_IDLE && tc_state === ts_IDLE) || mt_state === ms_IREL

  (0 until innerDataBeats).foreach(i => {
    i_data(i) := tgHelper.removeTag(xact.data_buffer(i))
    i_tag(i) := tgHelper.extractTag(xact.data_buffer(i))
  })

  // output acquire
  outer.acquire.valid := mt_state === ms_OACQ
  o_data := o_data.fromBits(i_data.toBits)
  outer.acquire.bits := PutBlock(
    client_xact_id = UInt(id),
    addr_block = xact.addr_block,
    addr_beat = oacq_cnt,
    data = o_data(oacq_cnt)
  )(p.alterPartial({ case TLId => outerTLId }))

  // input grant
  inner.grant.valid := mt_state === ms_IGNT && outer.grant.valid
  inner.grant.bits := coh.makeGrant(xact)
  when(mt_state === ms_OACQ && inner.grant.fire()) {
    ignt_done_reg := Bool(true)
  }
  outer.grant.ready := mt_state === ms_IGNT && inner.grant.ready

  // tag
  tc_req_valid := mt_state === ms_IDLE && inner.release.fire()

  when(mt_state === ms_IDLE && inner.release.fire()) {
    tc_wdata_valid := Bool(false)
    tc_xact.rw := Bool(true)
    tc_xact.addr := inner.release.bits.full_addr()
  }

  when(mt_state === ms_OACQ && !tc_wdata_valid) {
    tc_xact.mem_data := i_tag.toBits << (tc_tt_byte_index * UInt(8))
    tc_xact.mem_mask := (Vec.fill(tgHelper.cacheBlockTagBytes){Bool(true)}).toBits << tc_tt_byte_index
    tc_wdata_valid := Bool(true)
  }

  // ------------ memory tracker state machine ---------------- //
  when(mt_state === ms_IDLE && inner.release.fire()) {
    xact := inner.release.bits
    mt_state := Mux(irel_done, ms_IREL, ms_OACQ)
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
  val ms_IDLE :: ms_IACQ :: ms_OACQ :: ms_OGNT :: ms_IGNT :: ms_IFIN :: Nil = Num(UInt(), 6)
  val mt_state = Reg(init = ms_IDLE)

  val i_rows = innerDataBits / rowBits
  val o_rows = outerDataBits / rowBits
  val xact  = Reg(new AcquireFromSrc()(p.alterPartial({ case TLId => innerTLId })))
  val data  = Reg(Vec(refillCycles, UInt(width=rowBits)))
  val tag   = Reg(Vec(refillCycles, UInt(width=rowTagBits)))
  val tmask = Reg(Vec(refillCycles, UInt(width=rowTags)))

  val iacq_cnt = inner.acquire.bits.addr_beat
  val iacq_done = connectIncomingDataBeatCounter(inner.acquire)
  val (ignt_cnt, ignt_done) = connectOutgoingDataBeatCounter(inner.grant)
  val (oacq_cnt, oacq_done) = connectOutgoingDataBeatCounter(outer.acquire)
  val ognt_done = connectIncomingDataBeatCounter(outer.grant)

  // inner acquire
  when(inner.acquire.fire()) {
    val iacq_data = tgHelper.removeTag(inner.acquire.bits.data)
    val iacq_tag = tgHelper.extractTag(inner.acquire.bits.data)
    (0 until i_rows).foreach( i => {
      data(iacq_cnt*UInt(i_rows) + UInt(i)) := iacq_data(i*rowBits+rowBits-1,i*rowBits)
      tag(iacq_cnt*UInt(i_rows) + UInt(i)) := iacq_tag(i*rowTagBits+rowTagBits-1,i*rowTagBits)
    })
  }
  inner.acquire.ready : (mt_state === ms_IDLE && tc_state === ts_IDLE) || mt_state === ms_IACQ

  // outer acquire
  outer.acquire.valid := mt_state === ms_OACQ
  outer.acquire.bits := Acquire(
    is_builtin_type = Bool(true),
    a_type = Mux(xact.isBuiltInType(), xact.a_type, Acquire.getBlockType),
    client_xact_id = UInt(id),
    addr_block = xact.addr_block,
    addr_beat = oacq_cnt,
    data = (Vec((0 until o_rows).map(i => data(oacq_cnt*UInt(o_rows)+UInt(i))))).toBits,
    union = Mux(xact.isBuiltInType(),
      Acquire.makeUnion( // re-assemble union to rectify wmask
        a_type = xact.a_type,
        addr_byte = xact.addr_byte,
        operand_size = xact.op_size,
        opcode = xact.op_code,
        wmask = tgHelper.removeTagMask(xact.wmask),
        alloc = Bool(true)
      ),
      Cat(MT_Q, M_XRD, Bool(true)))) // coherent Acquire must be getBlock?

  // ------------ memory tracker state machine ---------------- //
  when(mt_state === ms_IDLE && inner.acquire.fire()) {
    xact := inner.acquire.bits
    mt_state := Mux(iacq_done, ms_IACQ, ms_OACQ)
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
    mt_state := Mux(xact.requiresAck(), ms_IFIN, ms_IDLE)
  }
  when(mt_state === ms_IFIN && inner.finish.valid)
    mt_state := ms_IDLE
  }
}


////////////////////////////////////////////////////////////////
// Top level of the Tag Cache

class TagCache(implicit p: Parameters) extends TCModule()(p) {
  val io = new ManagerTLIO

  val meta      = Module(new TCMetadataArray()(tcTagTLParams))
  val data      = Module(new TCDataArray()(tcTagTLParams))

  val memXactTrackerList =
    (0               until nReleaseTransactors).map(id => Module(new TCMemReleaseTracker(id))) ++
    (nReleaseTransactors until nMemTransactors).map(id => Module(new TCMemAcquireTracker(id)))

  val tagXactTrackerList = (nTagTransactors until nTagTransactors + nMemTransactors).map(id =>
    Module(new TCTagXactTracker(id)(tcTagTLParams)))

  val writeback = Module(new TCWritebackUnit(nTagTransactors + nMemTransactors)(tcTagTLParams))

}


/*
/////////////////////////////


{

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


 */
