// See LICENSE for license details.

package uncore
import Chisel._
import junctions._
import cde.{Parameters, Field}

case object TagBits extends Field[Int]
case object TCBlockBits extends Field[Int]
case object TCBlockTags extends Field[Int]
case object TCTransactors extends Field[Int]
case object TCBaseAddr extends Field[Int]

class TagCache(implicit val p: Parameters) extends Module {
  val io = new Bundle {
    val inner = new ManagerTileLinkIO
    val mem = new MemIO
  }

  io.mem.req_cmd.valid := io.inner.acquire.valid
  io.inner.acquire.ready := io.mem.req_cmd.ready
}
