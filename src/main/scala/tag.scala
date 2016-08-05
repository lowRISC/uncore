// See LICENSE for license details.

package uncore
import Chisel._
import junctions._
import cde.{Parameters, Field}
//import scala.collection.immutable.::
//import scala.math._

case object UseTagMem extends Field[Boolean]
case object TagMapRatio extends Field[BigInt]
case object TagBits extends Field[Int]
case object TCTransactors extends Field[Int]

abstract trait HasTagParameters {
  implicit val p: Parameters
  val tgBits = p(TagBits)                               // the number of bits in each tag
  val useTagMem = p(UseTagMem)
  val tgMapRatio = p(TagMapRatio)                       // the ratio of memory bytes to 1 bit in the bit map
  val tgHelper = TagUtil(tgBits)                        // tag helper functions

  val tgMapSize = tgHelper.minMapSize(p(RAMSize), tgMapRatio) // the size of the tag bit map
  val tgMemSize = tgHelper.minCacheSize(p(RAMSize)) + tgMapSize // the size of the tag memory partition
  val tgBaseAddr =                                      // the base address of the tag partition
    p(GlobalAddrHashMap)("mem").start + p(RAMSize) - tgMemSize
  val tgMapAddr =                                       // the base address of the tag bit map
    p(GlobalAddrHashMap)("mem").start + p(RAMSize) - tgMapSize

}

// support for tagged memory
class TagUtil(tagBits: Int) {
  def wordBits = 64                                     // add tag to every 64-bit word
  def wordBytes = wordBits / 8
  def tagWordBits = wordBits + tagBits                  // total size of tagged word
  def normTagBits = 1 << log2Up(tagBits)                // normalized tag bits (around up to the nears 2's power)
  def tagRatio = wordBits / normTagBits                 // tag compression ratio

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

  // convert physical address to tag address
  def pa2ta(addr: UInt): UInt = addr >> log2Up(tagRaio)

  // convert physical address to bitmap address
  def pa2ba(addr: UInt): UInt = addr >> log2Up(tagMapRatio * 8)

  // convert tag address to bitmap address
  def ta2ba(addr: UInt): UInt = addr >> (log2Up(tagMapRatio * 8) - log2Up(tagRaio))

  // calculate the minimal tag bit map size
  def minMapSize(mem: BigInt, tagMapRatio: BigInt) = mem / tagMapRatio / 8

  // calculate the minimal tag cache size
  def minCacheSize(mem: BigInt) = mem / tagRaio

}
