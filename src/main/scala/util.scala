// See LICENSE for license details.

package uncore

import Chisel._
import scala.math._

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
