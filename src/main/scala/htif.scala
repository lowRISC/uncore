// See LICENSE for license details.

package uncore

import Chisel._
import Chisel.ImplicitConversions._
import uncore._

abstract trait HTIFParameters extends UsesParameters {
  val nCores = params(NTiles)
  val nHosts = 1
  val nIDBits = max(nCores, nHosts)
  val xLen = params(XLen)
}

abstract class HTIFBundle extends Bundle with HTIFParameters
abstract class HTIFModule extends Module with HTIFParameters

class HostMsg extends HTIFBundle
{
  val id = UInt(width = log2Up(nCores))
  val data = UInt(width = xLen)
}

class HostIO extends HTIFBundle
{
  val req = Decoupled(new HostMsg)
  val resp = Decoupled(new HostMsg).flip
}

class HostNetworkIO extends HTIFBundle
{
  val req = new DecoupledIO(new PhysicalNetworkIO(nIDBits, new HostMsg))
  val resp = new DecoupledIO(new PhysicalNetworkIO(nIDBits, new HostMsg)).flip
}

/** Struct for describing per-channel queue depths */
case class HostDepths(req: Int, resp: Int)

/** Optionally enqueues each HostIO message individually */
class HostEnqueuer(depths: HostDepths) extends Module {
  val io = new Bundle {
    val client = new HostIO().flip
    val manager = new HostIO
  }
  io.manager.req <> (if(depths.req  > 0) Queue(io.client.req,   depths.req)  else io.client.req)
  io.client.resp <> (if(depths.resp > 0) Queue(io.manager.resp, depths.resp) else io.manager.resp)
}

class ClientHostNetworkPort extends HTIFModule {
  val io = new Bundle {
    val client = new HostIO().flip
    val network = new HostNetworkIO
  }

  io.network.req.valid := io.client.req.valid
  io.network.req.bits.payload := io.client.req.bits
  io.network.req.bits.header.dst := UInt(0)
  io.network.req.bits.header.src := io.client.req.bits.id
  io.client.req.ready := io.network.req.ready

  io.client.resp.valid := io.network.resp.valid
  io.client.resp.bits := io.network.resp.bits.payload
  io.network.resp.ready := io.client.resp.ready
}

class ManagerHostNetworkPort extends HTIFModule {
  val io = new Bundle {
    val manager = new HostIO
    val network = new HostNetworkIO().flip
  }

  io.network.resp.valid := io.manager.resp.valid
  io.network.resp.bits.payload := io.manager.resp.bits
  io.network.resp.bits.header.dst := io.manager.resp.bits.id
  io.network.resp.bits.header.src := UInt(0)
  io.manager.resp.ready := io.network.resp.ready

  io.manager.req.valid := io.network.req.valid
  io.manager.req.bits := io.network.req.bits.payload
  io.network.req.ready := io.manager.req.ready
}
