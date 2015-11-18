// See LICENSE for license details.

package uncore
import Chisel._
import scala.math.max

class PhysicalHeader(n: Int) extends Bundle {
  val src = UInt(width = log2Up(n))
  val dst = UInt(width = log2Up(n))
}

class PhysicalNetworkIO[T <: Data](n: Int, dType: T) extends Bundle {
  val header = new PhysicalHeader(n)
  val payload = dType.cloneType
  override def cloneType = new PhysicalNetworkIO(n,dType).asInstanceOf[this.type]
}

class BasicCrossbarIO[T <: Data](nIP: Int, nOP: Int, dType: T) extends Bundle {
  val nP  = max(nIP, nOP)
  val in  = Vec(new DecoupledIO(new PhysicalNetworkIO(nP,dType)), nIP).flip
  val out = Vec(new DecoupledIO(new PhysicalNetworkIO(nP,dType)), nOP)
}

abstract class PhysicalNetwork extends Module

class BasicCrossbar[T <: Data](nIP: Int, nOP: Int, dType: T, count: Int = 1, needsLock: Option[PhysicalNetworkIO[T] => Bool] = None) extends PhysicalNetwork {
  val io = new BasicCrossbarIO(nIP, nOP, dType)

  val rdyVecs = List.fill(nOP){Vec.fill(nIP)(Wire(Bool()))}

  io.out.zip(rdyVecs).zipWithIndex.map{ case ((out, rdys), i) => {
    val rrarb = Module(new LockingRRArbiter(io.in(0).bits, nIP, count, needsLock))
    (rrarb.io.in, io.in, rdys).zipped.map{ case (arb, in, rdy) => {
      arb.valid := in.valid && (in.bits.header.dst === UInt(i)) 
      arb.bits := in.bits
      rdy := arb.ready && (in.bits.header.dst === UInt(i))
    }}
    out <> rrarb.io.out
  }}
  for(i <- 0 until nIP) {
    io.in(i).ready := rdyVecs.map(r => r(i)).reduceLeft(_||_)
  }
}

abstract class LogicalNetwork extends Module

class LogicalHeader extends Bundle {
  val src = UInt(width = params(LNHeaderBits))
  val dst = UInt(width = params(LNHeaderBits))
}

class LogicalNetworkIO[T <: Data](dType: T) extends Bundle {
  val header = new LogicalHeader
  val payload = dType.cloneType
  override def cloneType = new LogicalNetworkIO(dType).asInstanceOf[this.type]
}

object DecoupledLogicalNetworkIOWrapper {
  def apply[T <: Data](
      in: DecoupledIO[T],
      src: UInt = UInt(0),
      dst: UInt = UInt(0)): DecoupledIO[LogicalNetworkIO[T]] = {
    val out = Wire(Decoupled(new LogicalNetworkIO(in.bits)))
    out.valid := in.valid
    out.bits.payload := in.bits
    out.bits.header.dst := dst
    out.bits.header.src := src
    in.ready := out.ready
    out
  }
}

object DecoupledLogicalNetworkIOUnwrapper {
  def apply[T <: Data](in: DecoupledIO[LogicalNetworkIO[T]]): DecoupledIO[T] = {
    val out = Wire(Decoupled(in.bits.payload))
    out.valid := in.valid
    out.bits := in.bits.payload
    in.ready := out.ready
    out
  }
}

object DefaultFromPhysicalShim {
  def apply[T <: Data](in: DecoupledIO[PhysicalNetworkIO[T]]): DecoupledIO[LogicalNetworkIO[T]] = {
    val out = Wire(Decoupled(new LogicalNetworkIO(in.bits.payload)))
    out.bits.header := in.bits.header
    out.bits.payload := in.bits.payload
    out.valid := in.valid
    in.ready := out.ready
    out
  }
}

object DefaultToPhysicalShim {
  def apply[T <: Data](n: Int, in: DecoupledIO[LogicalNetworkIO[T]]): DecoupledIO[PhysicalNetworkIO[T]] = {
    val out = Wire(Decoupled(new PhysicalNetworkIO(n, in.bits.payload)))
    out.bits.header := in.bits.header
    out.bits.payload := in.bits.payload
    out.valid := in.valid
    in.ready := out.ready
    out
  }
}
