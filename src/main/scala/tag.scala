// See LICENSE for license details.

package uncore
import Chisel._
import junctions._
import cde.{Parameters, Field}
//import scala.collection.immutable.::
//import scala.math._

case object UseTagMem extends Field[Boolean]
case object TagMapBits extends Field[Int]
case object TagBits extends Field[Int]

abstract trait HasTagParameters {
  implicit val p: Parameters
  val tgBits = p(TagBits)                               // the number of bits in each tag
  val tgMapBits = p(TagMapBits)                         // the number of bits in each tag map element
  val useTagMem = p(UseTagMem)

  val tgHelper = TagUtil(tgBits, tgMapBits,
    p(RAMSize), p(GlobalAddrHashMap)("mem").start,
    p(CacheBlockBytes))                                 // tag helper functions
}

// support for tagged memory
class TagUtil(
  tagBits: Int,                 // size of tag for each word
  mapBits: Int,                 // bit size of each element in tag map
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
  def mapRatio = (1 << (mapBits-1)) * cacheBlockBytes * 8 / mapBits
                                                        // map compression ratio
  def unMapBits = log2Up(mapRatio)                      // offset bits to get map address
  def cacheSize = memSize / tagRaio                     // size of tag cache
  def cacheBase = memBase + memSize - cacheSize         // base address of tag cache
  def mapSize = cacheSize / mapRatio                    // size of tag map
  def mapBase = memBase + memSize - mapSize             // base address of tag map

  require(isPow2(elemBits))                             // easy for address caclculation
  require(mapRatio > tagRatio)                          // no extra space for map 

  // remove the tags from a data line
  def removeTag(data: UInt): UInt = {
    require(data.getWidth >= tagWordBits && data.getWidth % tagWordBits == 0)
    val words = data.getWidth / tagWordBits
    (0 until words).map( i =>
      data(i*tagWordBits + wordBits - 1, i*tagWordBits)
    ).toBits
  }

  // extract the tags from a tagged data line
  def extractTag(data: UInt): UInt = {
    require(data.getWidth >= tagWordBits && data.getWidth % tagWordBits == 0)
    val words = data.getWidth / tagWordBits
    (0 until words).map( i =>
      tagData(i*tagWordBits + tagWordBits - 1, i*tagWordBits + wordBits)
    ).toBits
  }

  // Insert tags
  def insertTags(data: UInt, tags: UInt): UInt = {
    require(data.getWidth >= wordBits && data.getWidth % wordBits == 0)
    require(tags.getWidth == tagBits * data.getWidth / wordBits)
    val words = data.getWidth / wordBits
    (0 until words).map( i =>
      Cat(
        tags(i*tagBits + tagBits - 1, i*tagBits),
        rawData(i*wordBits + wordBits - 1, i*wordBits)
      )
    ).toBits
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
    (0 until words).map( i => {
      val wordMask = mask(i*wordBytes + wordBytes - 1, i*wordBytes)
      // write tag if any byte of the word is written
      Cat(wordMask.orR, wordMask)
    }).toBits
  }

  // insert rite mask for tags
  def insertTagMask(mask: UInt, tgMask: Bool = Bool(false)): UInt = {
    require(mask.getWidth >= wordBytes && mask.getWidth % wordBytes == 0)
    val words = mask.getWidth / wordBytes
    (0 until words).map( i => {
      Cat(tagMask, mask(i*wordBytes + wordBytes - 1, i*wordBytes))
    }).toBits
  }

  // remove tag mask
  def removeTagMask(mask: UInt): UInt = {
    require(mask.getWidth >= wordBytes + 1 && mask.getWidth % (wordBytes + 1) == 0)
    val words = mask.getWidth / (wordBytes + 1)
    (0 until words).map( i =>
      mask(i*(wordBytes+1) + wordBytes - 1, i*(wordBytes+1))
    ).toBits
  }

  // extract the tag write mask
  def extractTagMask(mask: UInt): UInt = {
    require(mask.getWidth >= wordBytes + 1 && mask.getWidth % (wordBytes + 1) == 0)
    val words = mask.getWidth / (wordBytes + 1)
    (0 until words).map(mask(_*wordBytes)).toBits
  }

  // calculate the extended size with tags
  def sizeWithTag(s: Int): Int = {
    require(s >= wordBits && s % wordBits == 0)
    s / wordBits * tagWordBits
  }

  // calculate the size without tags
  def sizeWithoutTag(s: Int): Int = {
    require(s >= tagWordBits && s % tagWordBits == 0)
    s / tagWordBits * wordBits
  }

  // calculate the extended mask size with tags
  def maskSizeWithTag(s: Int): Int = {
    require(s >= wordBytes && s % wordBytes == 0)
    s / wordByte * (wordBytes + 1)
  }

  // calculate the mask size without tags
  def maskSizeWithoutTag(s: Int): Int = {
    require(s >= wordBytes + 1 && s % (wordBytes + 1) == 0)
    s / (wordBytes + 1) * wordBytes
  }

  // convert physical address to tag address
  def pa2ta(addr: UInt): UInt = (addr >> unTagBits) + tagBase

  // convert physical address to map address
  def pa2ma(addr: UInt): UInt = (addr >> (unTagBits + unMapBits)) + mapBase
}
