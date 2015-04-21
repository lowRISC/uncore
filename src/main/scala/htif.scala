// See LICENSE for license details.

package uncore

import Chisel._
import Node._
import uncore._

case object HTIFWidth extends Field[Int]
case object HTIFNSCR extends Field[Int]
case object HTIFOffsetBits extends Field[Int]
case object HTIFNCores extends Field[Int]

abstract trait HTIFParameters extends UsesParameters {
  val dataBits = params(TLDataBits)
  val co = params(TLCoherence)
  val w = params(HTIFWidth)
  val nSCR = params(HTIFNSCR)
  val offsetBits = params(HTIFOffsetBits)
  val nCores = params(HTIFNCores)
}

abstract class HTIFBundle extends Bundle with HTIFParameters

class HostIO extends HTIFBundle
{
  val clk = Bool(OUTPUT)
  val clk_edge = Bool(OUTPUT)
  val in = Decoupled(Bits(width = w)).flip
  val out = Decoupled(Bits(width = w))
  val debug_stats_pcr = Bool(OUTPUT)
}

class PCRReq extends Bundle
{
  val rw = Bool()
  val addr = Bits(width =  8 ) //5) //allow more CSRs to be read by front-end server
  val data = Bits(width = 64)
}

class HTIFIO extends HTIFBundle {
  val reset = Bool(INPUT)
  val id = UInt(INPUT, log2Up(nCores))
  val pcr_req = Decoupled(new PCRReq).flip
  val pcr_rep = Decoupled(Bits(width = 64))
  val ipi_req = Decoupled(Bits(width = log2Up(nCores)))
  val ipi_rep = Decoupled(Bool()).flip
  val debug_stats_pcr = Bool(OUTPUT)
    // wired directly to stats register
    // expected to be used to quickly indicate to testbench to do logging b/c in 'interesting' work
}

class SCRIO extends HTIFBundle {
  val rdata = Vec.fill(nSCR){Bits(INPUT, 64)}
  val wen = Bool(OUTPUT)
  val waddr = UInt(OUTPUT, log2Up(nSCR))
  val wdata = Bits(OUTPUT, 64)
}

class HTIFModuleIO extends HTIFBundle {
    val host = new HostIO
    val cpu = Vec.fill(nCores){new HTIFIO}.flip
    val mem = new TileLinkIO
    val scr = new SCRIO
}

class HTIF(pcr_RESET: Int, pcr_TagBase: Int) extends Module with HTIFParameters {
  // conditional IO to support performance counter
  class IOBundle extends HTIFModuleIO

  class IOBundle_PFC extends IOBundle {
    val pfc = new CachePerformCounterReg().flip
    val tag_pfc_reset = Bool(OUTPUT)
  }

  val io = new IOBundle_PFC

  // tag utilities
  val tagUtil = new TagUtil(params(TagBits), params(CoreDataBits))

  // performance counters for tag cache
  val reg_Tag_write_cnt = Reg(init=UInt(0, params(PerformCounterBits)))
  val reg_Tag_write_miss_cnt = Reg(init=UInt(0, params(PerformCounterBits)))
  val reg_Tag_read_cnt = Reg(init=UInt(0, params(PerformCounterBits)))
  val reg_Tag_read_miss_cnt = Reg(init=UInt(0, params(PerformCounterBits)))
  val reg_Tag_write_back_cnt = Reg(init=UInt(0, params(PerformCounterBits)))
  val reg_tag_pfc_reset = Reg(init=Bool(false))

  if(params(UsePerformCounters)) {
    reg_Tag_write_cnt := io.pfc.write_cnt
    reg_Tag_write_miss_cnt := io.pfc.write_miss_cnt
    reg_Tag_read_cnt := io.pfc.read_cnt
    reg_Tag_read_miss_cnt := io.pfc.read_miss_cnt
    reg_Tag_write_back_cnt := io.pfc.write_back_cnt
    io.tag_pfc_reset := reg_tag_pfc_reset
  }


  io.host.debug_stats_pcr := io.cpu.map(_.debug_stats_pcr).reduce(_||_)
    // system is 'interesting' if any tile is 'interesting'

  val short_request_bits = 64
  val long_request_bits = short_request_bits + dataBits
  require(short_request_bits % w == 0)

  val rx_count_w = 13 + log2Up(64) - log2Up(w) // data size field is 12 bits
  val rx_count = Reg(init=UInt(0,rx_count_w))
  val rx_shifter = Reg(Bits(width = short_request_bits))
  val rx_shifter_in = Cat(io.host.in.bits, rx_shifter(short_request_bits-1,w))
  val next_cmd = rx_shifter_in(3,0)
  val cmd = Reg(Bits())
  val size = Reg(Bits())
  val pos = Reg(Bits())
  val seqno = Reg(Bits())
  val addr = Reg(Bits())
  when (io.host.in.valid && io.host.in.ready) {
    rx_shifter := rx_shifter_in
    rx_count := rx_count + UInt(1)
    when (rx_count === UInt(short_request_bits/w-1)) {
      cmd := next_cmd
      size := rx_shifter_in(15,4)
      pos := rx_shifter_in(15,4+offsetBits-3)
      seqno := rx_shifter_in(23,16)
      addr := rx_shifter_in(63,24)
    }
  }

  val rx_word_count = (rx_count >> UInt(log2Up(short_request_bits/w)))
  val rx_word_done = io.host.in.valid && rx_count(log2Up(short_request_bits/w)-1,0).andR
  val packet_ram_depth = long_request_bits/short_request_bits-1
  val packet_ram = Mem(Bits(width = short_request_bits), packet_ram_depth)
  when (rx_word_done && io.host.in.ready) {
    packet_ram(rx_word_count(log2Up(packet_ram_depth)-1,0) - UInt(1)) := rx_shifter_in
  }

  val cmd_readmem :: cmd_writemem :: cmd_readcr :: cmd_writecr :: cmd_ack :: cmd_nack :: Nil = Enum(UInt(), 6)

  val pcr_addr = addr(io.cpu(0).pcr_req.bits.addr.getWidth-1, 0)
  val pcr_coreid = addr(log2Up(nCores)-1+20+1,20)
  val pcr_wdata = packet_ram(0)

  val bad_mem_packet = size(offsetBits-1-3,0).orR || addr(offsetBits-1-3,0).orR
  val nack = Mux(cmd === cmd_readmem || cmd === cmd_writemem, bad_mem_packet,
             Mux(cmd === cmd_readcr || cmd === cmd_writecr, size != UInt(1),
             Bool(true)))

  val tx_count = Reg(init=UInt(0, rx_count_w))
  val tx_subword_count = tx_count(log2Up(short_request_bits/w)-1,0)
  val tx_word_count = tx_count(rx_count_w-1, log2Up(short_request_bits/w))
  val packet_ram_raddr = tx_word_count(log2Up(packet_ram_depth)-1,0) - UInt(1)
  when (io.host.out.valid && io.host.out.ready) {
    tx_count := tx_count + UInt(1)
  }

  val rx_done = rx_word_done && Mux(rx_word_count === UInt(0), next_cmd != cmd_writemem && next_cmd != cmd_writecr, rx_word_count === size || rx_word_count(log2Up(packet_ram_depth)-1,0) === UInt(0))
  val tx_size = Mux(!nack && (cmd === cmd_readmem || cmd === cmd_readcr || cmd === cmd_writecr), size, UInt(0))
  val tx_done = io.host.out.ready && tx_subword_count.andR && (tx_word_count === tx_size || tx_word_count > UInt(0) && packet_ram_raddr.andR)

  val mem_acked = Reg(init=Bool(false))
  val mem_gxid = Reg(Bits())
  val mem_gsrc = Reg(UInt())
  val mem_needs_ack = Reg(Bool())
  when (io.mem.grant.valid) { 
    mem_acked := Bool(true)
    mem_gxid := io.mem.grant.bits.payload.master_xact_id
    mem_gsrc := io.mem.grant.bits.header.src
    mem_needs_ack := co.requiresAckForGrant(io.mem.grant.bits.payload.g_type)
  }
  io.mem.grant.ready := Bool(true)

  val state_rx :: state_pcr_req :: state_pcr_resp :: state_mem_rreq :: state_mem_wreq :: state_mem_rresp :: state_mem_wresp :: state_mem_finish :: state_tx :: Nil = Enum(UInt(), 9)
  val state = Reg(init=state_rx)

  val rx_cmd = Mux(rx_word_count === UInt(0), next_cmd, cmd)
  when (state === state_rx && rx_done) {
    state := Mux(rx_cmd === cmd_readmem, state_mem_rreq,
             Mux(rx_cmd === cmd_writemem, state_mem_wreq,
             Mux(rx_cmd === cmd_readcr || rx_cmd === cmd_writecr, state_pcr_req,
             state_tx)))
  }

  val acq_q = Module(new Queue(new Acquire, 1))
  when (state === state_mem_wreq && acq_q.io.enq.ready) {
    state := state_mem_wresp
  }
  when (state === state_mem_rreq && acq_q.io.enq.ready) {
    state := state_mem_rresp
  }
  when (state === state_mem_wresp) {
    when (mem_acked) {
      state := state_mem_finish
      mem_acked := Bool(false)
    }
  }
  when (state === state_mem_rresp) {
    when (io.mem.grant.valid) {
      state := state_mem_finish
    }
    mem_acked := Bool(false)
  }
  when (state === state_mem_finish && io.mem.finish.ready) {
    state := Mux(cmd === cmd_readmem || pos === UInt(1),  state_tx, state_rx)
    pos := pos - UInt(1)
    addr := addr + UInt(1 << offsetBits-3)
  }
  when (state === state_tx && tx_done) {
    when (tx_word_count === tx_size) {
      rx_count := UInt(0)
      tx_count := UInt(0)
    }
    state := Mux(cmd === cmd_readmem && pos != UInt(0), state_mem_rreq, state_rx)
  }

  var mem_req_data: Bits = null
  val grant_payload_without_tag = tagUtil.removeTag(io.mem.grant.bits.payload.data)
  for (i <- 0 until dataBits/short_request_bits) {
    val idx = UInt(i, log2Up(dataBits/short_request_bits))
    when (state === state_mem_rresp && io.mem.grant.valid) {
      packet_ram(idx) := grant_payload_without_tag((i+1)*short_request_bits-1, i*short_request_bits)
    }
    mem_req_data = Cat(packet_ram(idx), mem_req_data)
  }
  acq_q.io.enq.valid := state === state_mem_rreq || state === state_mem_wreq
  val init_addr = addr.toUInt >> UInt(offsetBits-3)
  acq_q.io.enq.bits := Mux(cmd === cmd_writemem, 
    Acquire(co.getUncachedWriteAcquireType, init_addr, UInt(0)), 
    Acquire(co.getUncachedReadAcquireType, init_addr, UInt(0)))
  io.mem.acquire.valid := acq_q.io.deq.valid
  acq_q.io.deq.ready := io.mem.acquire.ready
  io.mem.acquire.bits.payload := acq_q.io.deq.bits
  io.mem.acquire.bits.payload.data := tagUtil.insertTag(mem_req_data)
  io.mem.acquire.bits.header.src := UInt(params(LNClients)) // By convention HTIF is the client with the largest id
  io.mem.acquire.bits.header.dst := UInt(0) // DNC; Overwritten outside module
  io.mem.finish.valid := (state === state_mem_finish) && mem_needs_ack
  io.mem.finish.bits.payload.master_xact_id := mem_gxid
  io.mem.finish.bits.header.dst := mem_gsrc
  io.mem.probe.ready := Bool(false)
  io.mem.release.valid := Bool(false)

  val pcr_reset = UInt(pcr_RESET)(pcr_addr.getWidth-1,0)

  // performance counters
  val pfc_Tag_write_cnt = Cat(UInt(pcr_TagBase)(pcr_addr.getWidth-1,4), UInt(0,4))
  val pfc_Tag_write_miss_cnt = Cat(UInt(pcr_TagBase)(pcr_addr.getWidth-1,4), UInt(1,4))
  val pfc_Tag_read_cnt = Cat(UInt(pcr_TagBase)(pcr_addr.getWidth-1,4), UInt(2,4))
  val pfc_Tag_read_miss_cnt = Cat(UInt(pcr_TagBase)(pcr_addr.getWidth-1,4), UInt(3,4))
  val pfc_Tag_write_back_cnt = Cat(UInt(pcr_TagBase)(pcr_addr.getWidth-1,4), UInt(4,4))
  val pfc_tag_pfc_reset = Cat(UInt(pcr_TagBase)(pcr_addr.getWidth-1,4), UInt(15,4))
  // end of performance counters

  val pcrReadData = Reg(Bits(width = io.cpu(0).pcr_rep.bits.getWidth))
  for (i <- 0 until nCores) {
    val my_reset = Reg(init=Bool(true))
    val my_ipi = Reg(init=Bool(false))

    val cpu = io.cpu(i)
    val me = pcr_coreid === UInt(i)
    cpu.pcr_req.valid := state === state_pcr_req && me && pcr_addr != pcr_reset &&
                         (if(params(UsePerformCounters))
                           pcr_addr(pcr_addr.getWidth-1,4) != UInt(15)
                         else
                           Bool(true))

    cpu.pcr_req.bits.rw := cmd === cmd_writecr
    cpu.pcr_req.bits.addr := pcr_addr
    cpu.pcr_req.bits.data := pcr_wdata
    cpu.reset := my_reset

    when (cpu.ipi_rep.ready) {
      my_ipi := Bool(false)
    }
    cpu.ipi_rep.valid := my_ipi
    cpu.ipi_req.ready := Bool(true)
    for (j <- 0 until nCores) {
      when (io.cpu(j).ipi_req.valid && io.cpu(j).ipi_req.bits === UInt(i)) {
        my_ipi := Bool(true)
      }
    }

    when (cpu.pcr_req.valid && cpu.pcr_req.ready) {
      state := state_pcr_resp
    }
    when (state === state_pcr_req && me && pcr_addr === pcr_reset) {
      when (cmd === cmd_writecr) {
        my_reset := pcr_wdata(0)
      }
      pcrReadData := my_reset.toBits
      state := state_tx
    }

    cpu.pcr_rep.ready := Bool(true)
    when (cpu.pcr_rep.valid) {
      pcrReadData := cpu.pcr_rep.bits
      state := state_tx
    }
  }

  if(params(UsePerformCounters)) {
    when(state === state_pcr_req && pcr_addr === pfc_Tag_write_cnt) {
      when(cmd === cmd_writecr) {
        reg_Tag_write_cnt := pcr_wdata
      }
      pcrReadData := reg_Tag_write_cnt
      state := state_tx
    }
    when(state === state_pcr_req && pcr_addr === pfc_Tag_write_miss_cnt) {
      when(cmd === cmd_writecr) {
        reg_Tag_write_miss_cnt := pcr_wdata
      }
      pcrReadData := reg_Tag_write_miss_cnt
      state := state_tx
    }
    when(state === state_pcr_req && pcr_addr === pfc_Tag_read_cnt) {
      when(cmd === cmd_writecr) {
        reg_Tag_read_cnt := pcr_wdata
      }
      pcrReadData := reg_Tag_read_cnt
      state := state_tx
    }
    when(state === state_pcr_req && pcr_addr === pfc_Tag_read_miss_cnt) {
      when(cmd === cmd_writecr) {
        reg_Tag_read_miss_cnt := pcr_wdata
      }
      pcrReadData := reg_Tag_read_miss_cnt
      state := state_tx
    }
    when(state === state_pcr_req && pcr_addr === pfc_Tag_write_back_cnt) {
      when(cmd === cmd_writecr) {
        reg_Tag_write_back_cnt := pcr_wdata
      }
      pcrReadData := reg_Tag_write_back_cnt
      state := state_tx
    }
    when(state === state_pcr_req && pcr_addr === pfc_tag_pfc_reset) {
      when(cmd === cmd_writecr) {
        reg_tag_pfc_reset := pcr_wdata(0)
      }
      pcrReadData := reg_tag_pfc_reset
      state := state_tx
    }
  }

  if(params(UsePerformCounters)) {
    when(state === state_pcr_req && pcr_addr === pfc_Tag_write_cnt) {
      when(cmd === cmd_writecr) {
        reg_Tag_write_cnt := pcr_wdata
      }
      pcrReadData := reg_Tag_write_cnt

    }
  }

  val scr_addr = addr(log2Up(nSCR)-1, 0)
  val scr_rdata = Vec.fill(io.scr.rdata.size){Bits(width = 64)}
  for (i <- 0 until scr_rdata.size)
    scr_rdata(i) := io.scr.rdata(i)
  scr_rdata(0) := UInt(nCores)
  scr_rdata(1) := UInt((BigInt(dataBits/8) << acq_q.io.enq.bits.addr.getWidth) >> 20)

  io.scr.wen := Bool(false)
  io.scr.wdata := pcr_wdata
  io.scr.waddr := scr_addr.toUInt
  when (state === state_pcr_req && pcr_coreid === SInt(-1)) {
    io.scr.wen := cmd === cmd_writecr
    pcrReadData := scr_rdata(scr_addr)
    state := state_tx
  }

  val tx_cmd = Mux(nack, cmd_nack, cmd_ack)
  val tx_cmd_ext = Cat(Bits(0, 4-tx_cmd.getWidth), tx_cmd)
  val tx_header = Cat(addr, seqno, tx_size, tx_cmd_ext)
  val tx_data = Mux(tx_word_count === UInt(0), tx_header,
                Mux(cmd === cmd_readcr || cmd === cmd_writecr, pcrReadData,
                packet_ram(packet_ram_raddr)))

  io.host.in.ready := state === state_rx
  io.host.out.valid := state === state_tx
  io.host.out.bits := tx_data >> Cat(tx_count(log2Up(short_request_bits/w)-1,0), Bits(0, log2Up(w)))
}
