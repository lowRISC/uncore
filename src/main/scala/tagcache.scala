// See LICENSE for license details.

package uncore
import Chisel._

class TagCache extends Module {
  val io = new Bundle {
    val inner = new ManagerTileLinkIO
    val mem = new MemIO
  }

  io.mem.req_cmd.valid := io.inner.acquire.valid
  io.inner.acquire.ready := io.mem.req_cmd.ready
}
