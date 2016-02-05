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

class FlowThroughSerializer[T <: HasTileLinkData](gen: T, n: Int) extends Module {
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

    val shifter = Vec(Bits(width = narrowWidth), n)
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
  def apply[T <: HasTileLinkData](in: DecoupledIO[T], n: Int): DecoupledIO[T] = {
    val fs = Module(new FlowThroughSerializer(in.bits, n))
    fs.io.in.valid := in.valid
    fs.io.in.bits := in.bits
    in.ready := fs.io.in.ready
    fs.io.out
  }
} 

class DecoupledPipe[T <: Data] (gen: T) extends Module {
  val io = new Bundle {
    val pi = Decoupled(gen.clone).flip
    val po = Decoupled(gen.clone)
  }

  val valid = Reg(init=Bool(false))
  val bits = Reg(gen.clone)

  io.pi.ready := !valid || io.po.ready
  io.po.valid := valid
  io.po.bits := bits

  when(io.pi.fire()) {
    valid := Bool(true)
    bits := io.pi.bits
  } .elsewhen(io.po.fire()) {
    valid := Bool(false)
  }

}

class SharedBuffer[T <: Data](gen: T, n: Int, size: Int, widths: List[Int]) extends Module {
  val width = widths.max
  val io = new Bundle {
    val in = Vec(n, Decoupled(Vec(width, UInt(width=8)))).flip
    val out = Vec(n, Decoupled(Vec(width, UInt(width=8))))
    val reset_in = Bool(INPUT) // reset write pointer for partial write
    val reset_out = Bool(INPUT) // reset read pointer for partial read
  }

  widths.foreach(w => require(size % w == 0))

  val pointerWidth = log2Up(size)
  val wp = Reg(init = UInt(0, width=pointerWidth))
  val rp = Reg(init = UInt(0, width=pointerWidth))
  val filled = Reg(init = Bool(false))
  val buf = Reg(Vec(size, UInt(width=8)))

  in.zip(widths).map { (ip, w)  => {
    when(ip.fire()) {
      when(Cat(UInt(0), wp) + UInt(w) >= UInt(size)) {
        filled := Bool(true)
        wp := UInt(0)
        buf(UInt(size-1), wp) := ip.bits
      }.otherwise{
        wp := wp + UInt(w)
        buf(wp+UInt(w-1), wp) := ip.bits
      }
    }
    ip.ready := !filled
  }}

  out.zip(widths).map{ (op, w) => {
    when(op.fire()) {
      when(Cat(UInt(0), rp) + UInt(w) >= UInt(size)) {
        filled := Bool(false)
        rp := UInt(0)
      }.otherwise{
        rp := rp + UInt(w)
      }
    }
    op.bits := Mux(
      Cat(UInt(0), rp) + UInt(w) >= UInt(size),
      buf(UInt(size-1), rp),
      buf(rp+UInt(w-1), rp))
    op.valid := filled || rp + UInt(w) <= wp
  }}

  when(reset_in) {
    filled := Bool(true)
    wp := UInt(0)
  }

  when(reset_out) {
    filled := Bool(false)
    rp := UInt(0)
  }

}
