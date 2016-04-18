// See LICENSE for license details.

package uncore

import Chisel._
import scala.math._

class Unsigned(x: Int) {
  require(x >= 0)
  def clog2: Int = { require(x > 0); ceil(log(x)/log(2)).toInt }
  def log2: Int = { require(x > 0); floor(log(x)/log(2)).toInt }
  def isPow2: Boolean = x > 0 && (x & (x-1)) == 0
  def nextPow2: Int = if (x == 0) 1 else 1 << clog2
}

object MuxBundle {
  def apply[T <: Data] (default: T, mapping: Seq[(Bool, T)]): T = {
    mapping.reverse.foldLeft(default)((b, a) => Mux(a._1, a._2, b))
  }

  def apply[T <: Data] (key: UInt, default: T, mapping: Seq[(UInt, T)]): T = {
    apply(default, mapping.map{ case (a, b) => (a === key, b) })
  }
}

// Produces 0-width value when counting to 1
class ZCounter(val n: Int) {
  val value = Reg(init=UInt(0, log2Ceil(n)))
  def inc(): Bool = {
    if (n == 1) Bool(true)
    else {
      val wrap = value === UInt(n-1)
      value := Mux(Bool(!isPow2(n)) && wrap, UInt(0), value + UInt(1))
      wrap
    }
  }
}

object ZCounter {
  def apply(n: Int) = new ZCounter(n)
  def apply(cond: Bool, n: Int): (UInt, Bool) = {
    val c = new ZCounter(n)
    var wrap: Bool = null
    when (cond) { wrap = c.inc() }
    (c.value, cond && wrap)
  }
}

object TwoWayCounter {
  def apply(up: Bool, down: Bool, max: Int): UInt = {
    val cnt = Reg(init = UInt(0, log2Up(max+1)))
    when (up && !down) { cnt := cnt + UInt(1) }
    when (down && !up) { cnt := cnt - UInt(1) }
    cnt
  }
}

class FlowThroughSerializer[T <: Bundle with HasTileLinkData](gen: T, n: Int) extends Module {
  val io = new Bundle {
    val in = Decoupled(gen).flip
    val out = Decoupled(gen)
    val cnt = UInt(OUTPUT, log2Up(n))
    val done = Bool(OUTPUT)
  }
  val narrowWidth = io.in.bits.data.getWidth / n
  require(io.in.bits.data.getWidth % narrowWidth == 0)

  if(n == 1) {
    io.out <> io.in
    io.cnt := UInt(0)
    io.done := Bool(true)
  } else {
    val cnt = Reg(init=UInt(0, width = log2Up(n)))
    val wrap = cnt === UInt(n-1)
    val rbits = Reg{io.in.bits}
    val active = Reg(init=Bool(false))

    val shifter = Wire(Vec(n, Bits(width = narrowWidth)))
    (0 until n).foreach { 
      i => shifter(i) := rbits.data((i+1)*narrowWidth-1,i*narrowWidth)
    }

    io.done := Bool(false)
    io.cnt := cnt
    io.in.ready := !active
    io.out.valid := active || io.in.valid
    io.out.bits := io.in.bits
    when(!active && io.in.valid) {
      when(io.in.bits.hasData()) {
        cnt := Mux(io.out.ready, UInt(1), UInt(0))
        rbits := io.in.bits
        active := Bool(true)
      }
      io.done := !io.in.bits.hasData()
    }
    when(active) {
      io.out.bits := rbits
      io.out.bits.data := shifter(cnt)
      when(io.out.ready) { 
        cnt := cnt + UInt(1)
        when(wrap) {
          cnt := UInt(0)
          io.done := Bool(true)
          active := Bool(false)
        }
      }
    }
  }
}

object FlowThroughSerializer {
  def apply[T <: Bundle with HasTileLinkData](in: DecoupledIO[T], n: Int): DecoupledIO[T] = {
    val fs = Module(new FlowThroughSerializer(in.bits, n))
    fs.io.in.valid := in.valid
    fs.io.in.bits := in.bits
    in.ready := fs.io.in.ready
    fs.io.out
  }

class SerDesBuffer(dw: Int, size: Int, ipw: Int, opw: Int) extends Module {
  val pointerWidth = log2Up(size) + 1
  val io = new Bundle {
    val in = Decoupled(UInt(width=ipw*dw)).flip
    val in_size = UInt(INPUT, width=log2Up(ipw+1))
    val out = Decoupled(UInt(width=opw*dw))
    val out_size = UInt(INPUT, width=log2Up(opw+1))
    val count = UInt(OUTPUT, width=pointerWidth)
  }

  val wp = Reg(init = UInt(0, width=pointerWidth))
  val wp_w = Wire(UInt(width=pointerWidth))
  val wp_r = Wire(UInt(width=pointerWidth))
  val buffer = Reg(init = UInt(0, width=size*dw))
  val buffer_w = Wire(UInt(width=(size+ipw)*dw))
  val din_mask = Wire(UInt(width=ipw*dw))
  val buffer_r = Wire(UInt(width=size*dw))
  val size_u = UInt(size)
  val dw_u = UInt(dw)
  val ipw_u = UInt(ipw)
  val opw_u = UInt(opw)

  wp_w := Mux(io.in.fire(), wp + io.in_size, wp)
  din_mask := FillInterleaved(dw, (UInt(1) << io.in_size) - UInt(1))
  buffer_w := Mux(io.in.fire(), ((io.in.bits & din_mask) << (wp * dw_u)) | buffer, buffer)

  wp_r := Mux(io.out.fire(), wp_w - io.out_size, wp_w)
  buffer_r := buffer_w(size*dw-1,0) >> Mux(io.out.fire(), (io.out_size * dw_u), UInt(0))

  wp := wp_r
  buffer := buffer_r

  io.in.ready := wp + ipw_u <= size_u
  io.out.valid := io.out_size =/= UInt(0) && wp >= io.out_size
  io.count := wp
  io.out.bits := buffer
}
