// See LICENSE for license details.

package uncore
import Chisel._
import scala.math._

trait MIFParameters extends UsesParameters {
  val mifTagBits = params(MIFTagBits)
  val mifAddrBits = params(PAddrBits) - log2Up(params(MIFDataBits)/8)
  val mifDataBits = params(MIFDataBits)
  require(isPow2(params(MIFDataBits)))
  require(params(MIFDataBits) >= 8)
}
 
abstract class MIFBundle extends Bundle with MIFParameters
abstract class MIFModule extends Module with MIFParameters

trait HasMemData extends MIFBundle {
  val data = Bits(width = mifDataBits)
}

trait HasMemAddr extends MIFBundle {
  val addr = UInt(width = mifAddrBits)
}

trait HasMemTag extends MIFBundle {
  val tag = UInt(width = mifTagBits)
}

class MemReqCmd extends HasMemAddr with HasMemTag {
  val rw = Bool()
}

class MemTag extends HasMemTag
class MemData extends HasMemData
class MemResp extends HasMemData with HasMemTag

class MemIO extends Bundle {
  val req_cmd  = Decoupled(new MemReqCmd)
  val req_data = Decoupled(new MemData)
  val resp     = Decoupled(new MemResp).flip
}
