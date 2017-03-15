// See LICENSE for license details.

package uncore
import Chisel._
import junctions._
import cde.{Parameters, Field}
//import scala.collection.immutable.::
//import scala.math._

case object UseTagMem extends Field[Boolean]
case object TagBits extends Field[Int]
case object TagMapRatio extends Field[Int]

abstract trait HasTagParameters {
  implicit val p: Parameters
  val tgBits = p(TagBits)                               // the number of bits in each tag
  val tgInstBits = 2                                    // size of instruction tag
  val tgMapRatio = p(TagMapRatio)                       // the number of bits a map bit represents
  val useTagMem = p(UseTagMem)

  val tgHelper = new TagUtil(tgBits, tgMapRatio,
    p(RAMSize), p(GlobalAddrHashMap)("mem").start,
    p(CacheBlockBytes))                                 // tag helper functions

  require(!useTagMem || ((tgBits >= tgInstBits * 2) && (tgBits % tgInstBits == 0)))

}

// support for tagged memory
class TagUtil(
  tagBits: Int,                 // size of tag for each word
  mapRatio: Int,                // tag map compression ratio
  memSize: BigInt,              // size of tagged memory
  memBase: BigInt,              // base address of tagged memory
  cacheBlockBytes: Int = 64     // byte size of a cache block 
) {
  def wordBits = 64                                     // add tag to every 64-bit word
  def wordBytes = wordBits / 8
  def normTagBits = 1 << log2Up(tagBits)                // normalized tag bits (around up to the nears 2's power)
  def tagRatio = wordBits / normTagBits                 // tag compression ratio
  def unTagBits = log2Up(tagRatio)                      // offset bits to get tag address
  def unMapBits = log2Up(mapRatio)                      // offset bits to get map address
  def tableSize = memSize / tagRatio                    // size of the tag table
  def tableBase = memBase + memSize - tableSize         // base address of the tag table
  def map0Size  = tableSize / mapRatio                  // size of tag map 0
  def map0Base  = memBase + memSize - map0Size          // base address of tag map 0
  def map1Size  = map0Size / mapRatio                   // size of tag map 1
  def map1Base  = memBase + memSize - map1Size          // base address of tag map 1
  def cacheBlockTagBits = cacheBlockBytes / wordBytes * normTagBits // tag size of a cache block
  def cacheBlockTagBytes = cacheBlockTagBits / 8
  def blockOffBits = log2Up(cacheBlockBytes)
  def topSize = map1Size                                // size of the top map
  def topBase = map1Base                                // base address of the top map

  require(isPow2(memSize))
  require(isPow2(mapRatio))
  require(mapRatio >= tagRatio)                         // no extra space for map

  def tagSize(dataSize:Int) = dataSize / wordBits * tagBits
  def tagMaskSize(dataSize:Int) = dataSize / wordBits

  // convert physical address to tag table address / row byte index
  def pa2tta(addr: UInt): UInt = ((addr - UInt(memBase)) >> unTagBits) + UInt(tableBase)
  def pa2ttr(addr: UInt, rbo: Int): UInt = addr(unTagBits + rbo - 1, unTagBits)

  // convert physical address to tag map 0 address(a) / bit offset(b)
  def pa2tm0a(addr: UInt): UInt = ((addr - UInt(memBase)) >> (unTagBits + unMapBits)) + UInt(map0Base)
  def pa2tm0b(addr: UInt, rbo: Int): UInt = addr(unTagBits + unMapBits + rbo - 1, unTagBits + unMapBits - 3)

  // convert physical address to tag map 1 address(a) / bit offset(b)
  def pa2tm1a(addr: UInt): UInt = ((addr - UInt(memBase)) >> (unTagBits + unMapBits + unMapBits)) + UInt(map1Base)
  def pa2tm1b(addr: UInt, rbo: Int): UInt = addr(unTagBits + unMapBits + unMapBits + rbo - 1, unTagBits + unMapBits + unMapBits - 3)

  // now need to enforce writeback of tm1 and tm0
  def is_top(addr: UInt): Bool = (addr >> blockOffBits) >= UInt(map0Base >> blockOffBits)
}
