// See LICENSE for license details.

package uncore

import Chisel._
import scala.math._

case object UsePerformCounters extends Field[Boolean]
case object PerformCounterBits extends Field[Int]
case object MatchSpike extends Field[Boolean]
case object DebugPrint extends Field[Boolean]

// support for tagged memory
class TagUtil(tagBits: Int, dataBits: Int) {

  // remove the tags from a data line
  def removeTag(tagData: Bits): Bits = {
    val tagDataBits = tagBits + dataBits
    val words = tagData.getWidth / tagDataBits
    val raw_data = (0 until words).map(i => tagData(i*tagDataBits + dataBits - 1, i*tagDataBits))
    Vec(raw_data).toBits
  }

  // extract the tags from a tagged data line
  def extractTag(tagData: Bits): Bits = {
    val tagDataBits = tagBits + dataBits
    val words = tagData.getWidth / tagDataBits
    val tags = (0 until words).map(i =>
      tagData(i*tagDataBits + tagDataBits - 1, i*tagDataBits + dataBits))
    Vec(tags).toBits
  }

  // Insert default tag to a data line
  def insertTag(rawData: Bits, tags: Bits): Bits = {
    val words = rawData.getWidth / dataBits
    val tag_data = (0 until words).map(i =>
      Cat(
        tags(i*tagBits + tagBits - 1, i*tagBits),
        rawData(i*dataBits + dataBits - 1, i*dataBits)
      )
    )
    Vec(tag_data).toBits
  }

  // Insert empty tag
  def insertTag(rawData: Bits): Bits = {
    val words = rawData.getWidth / dataBits
    val defaultTag = Bits(0,tagBits*words)
    insertTag(rawData, defaultTag)
  }

  // insert corresponding write mask for L2
  def insertTagMask(mask: Bits): Bits = {
    val wordBytes = dataBits / 8
    val words = mask.getWidth / wordBytes
    val coreMask = (0 until words).map(i => mask(i*wordBytes + wordBytes - 1, i*wordBytes))
    // assuming write tag when any byte in the coreData line is being written
    val tagMask = (0 until words).map(i => Fill(tagBits, coreMask(i).orR()))
    val combinedMask = (0 until words).map(i => Cat(tagMask(i), FillInterleaved(8, coreMask(i))))
    Vec(combinedMask).toBits
  }

}


trait CachePerformCounterParameters extends UsesParameters {
  val counterBits = params(PerformCounterBits)
}

class CachePerformCounterInput extends Bundle {
  val write = Bool(INPUT)
  val write_miss = Bool(INPUT)
  val read = Bool(INPUT)
  val read_miss = Bool(INPUT)
  val write_back = Bool(INPUT)
}

class CachePerformCounterReg extends Bundle with CachePerformCounterParameters {
  val write_cnt = UInt(OUTPUT, counterBits)
  val write_miss_cnt = UInt(OUTPUT, counterBits)
  val read_cnt = UInt(OUTPUT, counterBits)
  val read_miss_cnt = UInt(OUTPUT, counterBits)
  val write_back_cnt = UInt(OUTPUT, counterBits)
}

class CachePerformCounters extends Module with CachePerformCounterParameters {

  val io = new Bundle {
    val req = new CachePerformCounterInput
    val reg = new CachePerformCounterReg
    val pfc_reset = Bool(INPUT)
  }

  val requests = Vec(io.req.write, io.req.write_miss, io.req.read, io.req.read_miss, io.req.write_back)

  val counters = Vec.fill(5)(Reg(init=UInt(0,counterBits)))

  val outputs = Vec(io.reg.write_cnt, io.reg.write_miss_cnt, io.reg.read_cnt, io.reg.read_miss_cnt, io.reg.write_back_cnt)

  requests zip counters zip outputs map { case ((r,c),o) =>
    when(r) {
      c := c + UInt(1)
    }
    when(io.pfc_reset) { c := UInt(0) }
    o := c
  }

}
