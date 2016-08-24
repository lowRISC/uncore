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
  val tgMapRatio = p(TagMapRatio)                       // the number of bits a map bit represents
  val useTagMem = p(UseTagMem)

  val tgHelper = new TagUtil(tgBits, tgMapRatio,
    p(RAMSize), p(GlobalAddrHashMap)("mem").start,
    p(CacheBlockBytes))                                 // tag helper functions
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
  def tagWordBits = wordBits + tagBits                  // total size of tagged word
  def normTagBits = 1 << log2Up(tagBits)                // normalized tag bits (around up to the nears 2's power)
  def tagRatio = wordBits / normTagBits                 // tag compression ratio
  def unTagBits = log2Up(tagRatio)                      // offset bits to get tag address
  def unMapBits = log2Up(mapRatio)                      // offset bits to get map address
  def tableSize = memSize / tagRatio                     // size of the tag table
  def tableBase = memBase + memSize - tableSize         // base address of the tag table
  def map0Size  = tableSize / mapRatio                  // size of tag map 0
  def map0Base  = memBase + memSize - map0Size          // base address of tag map 0
  def map1Size  = map0Size / mapRatio                   // size of tag map 1
  def map1Base  = memBase + memSize - map1Size          // base address of tag map 1

  require(isPow2(mapRatio))
  require(mapRatio >= tagRatio)                         // no extra space for map

  // remove the tags from a data line
  def removeTag(data: UInt): UInt = {
    require(data.getWidth >= tagWordBits && data.getWidth % tagWordBits == 0)
    val words = data.getWidth / tagWordBits
    Vec((0 until words).map(i => data(i*tagWordBits + wordBits - 1, i*tagWordBits))).toBits
  }

  // extract the tags from a tagged data line
  def extractTag(data: UInt): UInt = {
    require(data.getWidth >= tagWordBits && data.getWidth % tagWordBits == 0)
    val words = data.getWidth / tagWordBits
    Vec((0 until words).map( i =>
      data(i*tagWordBits + tagWordBits - 1, i*tagWordBits + wordBits)
    )).toBits
  }

  // Insert tags
  def insertTags(data: UInt, tags: UInt): UInt = {
    require(data.getWidth >= wordBits && data.getWidth % wordBits == 0)
    require(tags.getWidth == tagBits * data.getWidth / wordBits)
    val words = data.getWidth / wordBits
    Vec((0 until words).map( i =>
      Cat(
        tags(i*tagBits + tagBits - 1, i*tagBits),
        data(i*wordBits + wordBits - 1, i*wordBits)
      )
    )).toBits
  }

  // Insert a fixed tag to all words
  def insertTag(data: UInt, tag: UInt = UInt(0,tagBits)): UInt = {
    require(data.getWidth >= wordBits && data.getWidth % wordBits == 0)
    require(tag.getWidth == tagBits)
    val words = data.getWidth / wordBits
    insertTag(data, Fill(words,tag))
  }

  // insert corresponding write mask for tags
  def insertTagMaskAuto(mask: UInt): UInt = {
    require(mask.getWidth >= wordBytes && mask.getWidth % wordBytes == 0)
    val words = mask.getWidth / wordBytes
    Vec((0 until words).map( i => {
      val wordMask = mask(i*wordBytes + wordBytes - 1, i*wordBytes)
      // write tag if any byte of the word is written
      Cat(wordMask.orR, wordMask)
    })).toBits
  }

  // insert rite mask for tags
  def insertTagMask(mask: UInt, tgMask: Bool = Bool(false)): UInt = {
    require(mask.getWidth >= wordBytes && mask.getWidth % wordBytes == 0)
    val words = mask.getWidth / wordBytes
    Vec((0 until words).map( i => {
      Cat(tgMask, mask(i*wordBytes + wordBytes - 1, i*wordBytes))
    })).toBits
  }

  // remove tag mask
  def removeTagMask(mask: UInt): UInt = {
    require(mask.getWidth >= wordBytes + 1 && mask.getWidth % (wordBytes + 1) == 0)
    val words = mask.getWidth / (wordBytes + 1)
    Vec((0 until words).map( i =>
      mask(i*(wordBytes+1) + wordBytes - 1, i*(wordBytes+1))
    )).toBits
  }

  // extract the tag write mask
  def extractTagMask(mask: UInt): UInt = {
    require(mask.getWidth >= wordBytes + 1 && mask.getWidth % (wordBytes + 1) == 0)
    val words = mask.getWidth / (wordBytes + 1)
    Vec((0 until words).map(i => mask(i*wordBytes))).toBits
  }

  // calculate the extended size with tags
  def sizeWithTag(s: Int): Int = {
    require(s >= wordBits && s % wordBits == 0)
    s / wordBits * tagWordBits
  }

  def sizeWithTag(s: UInt): UInt = {
    s / UInt(wordBits) * UInt(tagWordBits)
  }

  // calculate the size without tags
  def sizeWithoutTag(s: Int): Int = {
    require(s >= tagWordBits && s % tagWordBits == 0)
    s / tagWordBits * wordBits
  }

  // calculate the extended mask size with tags
  def maskSizeWithTag(s: Int): Int = {
    require(s >= wordBytes && s % wordBytes == 0)
    s / wordBytes * (wordBytes + 1)
  }

  // calculate the mask size without tags
  def maskSizeWithoutTag(s: Int): Int = {
    require(s >= wordBytes + 1 && s % (wordBytes + 1) == 0)
    s / (wordBytes + 1) * wordBytes
  }

  // convert physical address to tag table address
  def pa2tta(addr: UInt): UInt = (addr >> unTagBits) + UInt(tableBase)

  // convert physical address to tag map 0 address(a) / bit offset(b)
  def pa2tm0a(addr: UInt): UInt = (addr >> (unTagBits + unMapBits)) + UInt(map0Base)
  def pa2tm0b(addr: UInt): UInt = addr(unTagBits + unMapBits - 1, unTagBits + unMapBits - 3)

  // convert physical address to tag map 1 address(a) / bit offset(b)
  def pa2tm1a(addr: UInt): UInt = (addr >> (unTagBits + unMapBits + unMapBits)) + UInt(map1Base)
  def pa2tm1b(addr: UInt): UInt = addr(unTagBits + unMapBits + unMapBits - 1, unTagBits + unMapBits + unMapBits - 3)
}
