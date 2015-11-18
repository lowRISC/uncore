// See LICENSE for license details.

package uncore

import Chisel._
import Chisel.ImplicitConversions._
import uncore._
import scala.math.max

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
