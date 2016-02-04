package uncore

import Chisel._
import junctions._
import open_soc_debug._
import cde.Parameters

class TileLinkIOMamIOConverter(implicit p: Parameters) extends TLModule()(p)
    with HasMamParameters {

  val io = new Bundle {
    val mam = new MemIO.flip
    val tl = new ClientUncachedTileLinkIO
  }
}
