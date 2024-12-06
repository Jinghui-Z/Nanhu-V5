/***************************************************************************************
  * Copyright (c) 2020-2021 Institute of Computing Technology, Chinese Academy of Sciences
  * Copyright (c) 2020-2021 Peng Cheng Laboratory
  *
  * XiangShan is licensed under Mulan PSL v2.
  * You can use this software according to the terms and conditions of the Mulan PSL v2.
  * You may obtain a copy of Mulan PSL v2 at:
  *          http://license.coscl.org.cn/MulanPSL2
  *
  * THIS SOFTWARE IS PROVIDED ON AN "AS IS" BASIS, WITHOUT WARRANTIES OF ANY KIND,
  * EITHER EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO NON-INFRINGEMENT,
  * MERCHANTABILITY OR FIT FOR A PARTICULAR PURPOSE.
  *
  * See the Mulan PSL v2 for more details.
  ***************************************************************************************/

package xiangshan.frontend.icache

import org.chipsalliance.cde.config.Parameters
import chisel3._
import chisel3.util._
import xs.utils._
import xs.utils.perf._
import xiangshan.frontend.ExceptionType
import xiangshan.frontend.{FtqPtr, BpuFlushInfo}
import xiangshan.cache.mmu.Pbmt

/* WayLookupEntry is for internal storage, while WayLookupInfo is for interface
 * Notes:
 *   1. there must be a flush (caused by guest page fault) after excp_tlb_gpf === true.B,
 *      so, we need only the first excp_tlb_gpf and the corresponding gpaddr.
 *      to save area, we separate those signals from WayLookupEntry and store only once.
 */
class WayLookupEntry(implicit p: Parameters) extends ICacheBundle {
  val valid          : Bool = Bool()
  val vSetIdx        : Vec[UInt] = Vec(PortNumber, UInt(idxBits.W))
  val waymask        : Vec[UInt] = Vec(PortNumber, UInt(nWays.W))
  val ptag           : Vec[UInt] = Vec(PortNumber, UInt(tagBits.W))
  val ftqIdx         : FtqPtr = new FtqPtr
}

class WayUpdateInfo(implicit p: Parameters) extends ICacheBundle {
  val wayPred        = Vec(PortNumber, Valid(UInt(log2Up(nWays).W)))
  val ftqIdx         = new FtqPtr
}

class WayLookupInfo(implicit p: Parameters) extends ICacheBundle {
  val entry = new WayLookupEntry

  // for compatibility
  def valid          : Bool = entry.valid
  def vSetIdx        : Vec[UInt] = entry.vSetIdx
  def waymask        : Vec[UInt] = entry.waymask
  def ptag           : Vec[UInt] = entry.ptag
  def ftqIdx         : FtqPtr = entry.ftqIdx
}

class WriteWayLookupInfo(implicit p: Parameters) extends ICacheBundle {
  val entry = new WayLookupEntry
  val isDoubleLine = Bool()

  // for compatibility
  def valid          : Bool = entry.valid
  def vSetIdx        : Vec[UInt] = entry.vSetIdx
  def waymask        : Vec[UInt] = entry.waymask
  def ptag           : Vec[UInt] = entry.ptag
  def ftqIdx         : FtqPtr = entry.ftqIdx
}

class WayLookupInterface(implicit p: Parameters) extends ICacheBundle {
  val flush   = Input(Bool())
  val readFtqIdx = Input(new FtqPtr)
  val read    = DecoupledIO(new WayLookupInfo)
  val write   = Flipped(DecoupledIO(new WriteWayLookupInfo))
  val predWrite   = Flipped(DecoupledIO(new WayLookupInfo))
  val flushFromBpu = Flipped(new BpuFlushInfo)
  val update  = Flipped(ValidIO(new ICacheMissResp))
  val wayFlushS0 = Output(Bool())
  val wayFlushS1 = Output(Bool())
  val wayUpdate = Output(Valid(new WayUpdateInfo))
}

class WayLookup(implicit p: Parameters) extends ICacheModule {
  val io: WayLookupInterface = IO(new WayLookupInterface)

  class WayLookupPtr(implicit p: Parameters) extends CircularQueuePtr[WayLookupPtr](nWayLookupSize)

  private val entries       = RegInit(VecInit(Seq.fill(nWayLookupSize)(0.U.asTypeOf(new WayLookupEntry))))
  private val readPtr       = RegInit(0.U.asTypeOf(new WayLookupPtr))
  // private val readPtrPlus1  = RegInit(1.U.asTypeOf(new WayLookupPtr))
  // private val readPtrPlus2  = RegInit(2.U.asTypeOf(new WayLookupPtr))
  private val writePtr      = RegInit(0.U.asTypeOf(new WayLookupPtr))
  private val predWritePtr  = RegInit(0.U.asTypeOf(new WayLookupPtr))
  private val pre_predWritePtr  = RegInit(0.U.asTypeOf(new WayLookupPtr))
  private val wayFLushedFir = RegInit(false.B)
  private val wayFLushedSec = RegInit(false.B)

  private val empty = (readPtr.value === writePtr.value) && (readPtr.flag === writePtr.flag)
  private val pred_empty = (readPtr.value === predWritePtr.value) && (readPtr.flag === predWritePtr.flag)
  private val full  = (readPtr.value === writePtr.value) && (readPtr.flag ^ writePtr.flag)
  private val pred_full  = (readPtr.value === predWritePtr.value) && (readPtr.flag ^ predWritePtr.flag)
  
  when(io.flush) {
    predWritePtr     := 0.U.asTypeOf(new WayLookupPtr)
    pre_predWritePtr := 0.U.asTypeOf(new WayLookupPtr)
  }.elsewhen(io.predWrite.fire) {
    predWritePtr := predWritePtr + 1.U
    when(!pred_empty) {
      pre_predWritePtr := predWritePtr
    }
  }.elsewhen(io.flushFromBpu.shouldFlushByStage3(entries(pre_predWritePtr.value).ftqIdx) && !pred_empty) {
    predWritePtr := pre_predWritePtr
  }

  when(io.flush) {
    writePtr.value := 0.U
    writePtr.flag  := false.B
  }.elsewhen(io.write.fire) {
    writePtr := writePtr + 1.U
  }

  when(io.flush) {
    readPtr       := 0.U.asTypeOf(new WayLookupPtr)
    io.wayFlushS1 := false.B
    io.wayFlushS0 := false.B
    entries.zipWithIndex.foreach{ case(entry, i) =>
      entry.valid := false.B
    }
  }.elsewhen(io.read.fire) {
    readPtr := readPtr + 1.U
  }

  when(io.wayFlushS1 || io.wayFlushS0 || (!wayFLushedFir && !wayFLushedSec && pred_full && io.write.fire && !(io.write.bits.entry.ftqIdx > io.readFtqIdx))) {  // update readPtr if flush or pred_full, otherwise prefetch will stop
    readPtr := writePtr
  }

  when(io.flush || empty) {
    wayFLushedFir := false.B
    wayFLushedSec := false.B
  }

  /**
    ******************************************************************************
    * update
    ******************************************************************************
    */
  private val hits = Wire(Vec(nWayLookupSize, Bool()))
  entries.zip(hits).foreach{ case(entry, hit) =>
    val hit_vec = Wire(Vec(PortNumber, Bool()))
    (0 until PortNumber).foreach { i =>
      val vset_same = (io.update.bits.vSetIdx === entry.vSetIdx(i)) && !io.update.bits.corrupt && io.update.valid
      val ptag_same = getPhyTagFromBlk(io.update.bits.blkPaddr) === entry.ptag(i)
      val way_same = io.update.bits.waymask === entry.waymask(i)
      when(vset_same && entry.valid) {
        io.wayUpdate.valid := true.B 
        io.wayUpdate.bits.ftqIdx  := entry.ftqIdx
        when(ptag_same) {
          // miss -> hit
          entry.waymask(i) := io.update.bits.waymask
          io.wayUpdate.bits.wayPred(i).valid  := true.B
          io.wayUpdate.bits.wayPred(i).bits   := OHToUInt(io.update.bits.waymask)
          // also update meta_codes
          // we have getPhyTagFromBlk(io.update.bits.blkPaddr) === entry.ptag(i), so we can use entry.ptag(i) for better timing
          // entry.meta_codes(i) := encodeMetaECC(entry.ptag(i))
        }.elsewhen(way_same) {
          // data is overwritten: hit -> miss
          entry.waymask(i) := 0.U
          io.wayUpdate.bits.wayPred(i).valid  := false.B
          io.wayUpdate.bits.wayPred(i).bits   := 0.U(log2Up(nWays).W)
          // dont care meta_codes, since it's not used for a missed request
        }
      }
      hit_vec(i) := vset_same && (ptag_same || way_same)
    }
    hit := hit_vec.reduce(_||_)
  }

  /**
    ******************************************************************************
    * read
    ******************************************************************************
    */
  // if the entry is empty, but there is a valid write, we can bypass it to read port (maybe timing critical)
  io.read.valid := false.B
  io.read.bits.entry := entries(readPtr.value)
  entries.zipWithIndex.foreach{ case(entry, i) => 
    val ftqIdxSame = io.readFtqIdx === entry.ftqIdx
    when(ftqIdxSame && entry.valid) {
      io.read.valid := true.B
      io.read.bits.entry := entry
    }
  }

  /**
    ******************************************************************************
    * write
    ******************************************************************************
    */

  // pred way write
  io.predWrite.ready := !pred_full 
  when(io.predWrite.fire) {
    entries(predWritePtr.value) := io.predWrite.bits.entry
  }  

  val way_diff_vec = Wire(Vec(PortNumber, Bool()))
  // way prediction update
  (0 until PortNumber).foreach { i =>
    val way_diff_item = (io.write.bits.entry.waymask(i) =/= entries(writePtr.value).waymask(i)) && (io.write.bits.entry.ftqIdx === entries(writePtr.value).ftqIdx) && (predWritePtr =/= writePtr)
    way_diff_vec(i) := way_diff_item
    val hit = io.write.bits.entry.waymask(i).orR  
    io.wayUpdate.bits.wayPred(i).valid  := hit
    io.wayUpdate.bits.wayPred(i).bits   := Mux(hit, OHToUInt(io.write.bits.entry.waymask(i)), 0.U(log2Up(nWays).W))
  }
  // when(io.write.fire && way_diff_vec.reduce(_ || _)) {
  //   io.wayUpdate.valid := true.B
  // }
  // val way_diff = way_diff_vec(0) || (way_diff_vec(1) && io.write.bits.isDoubleLine)
  val way_diff = way_diff_vec.reduce(_ || _)
  io.wayFlushS0 := false.B
  io.wayFlushS1 := false.B
  io.wayUpdate.valid := false.B
  io.wayUpdate.bits.ftqIdx  := io.write.bits.entry.ftqIdx

  io.write.ready := !full 
  when(io.write.fire) {
    when(way_diff) {
      io.wayUpdate.valid := true.B 
      when((!wayFLushedFir || empty) && io.write.bits.entry.ftqIdx < io.readFtqIdx) {
        io.wayFlushS1 := true.B
        wayFLushedFir := true.B
      }
      when((!wayFLushedSec || empty) && io.write.bits.entry.ftqIdx === io.readFtqIdx) {
        wayFLushedSec := true.B
        io.wayFlushS0 := true.B
      }
    }
    // }.elsewhen(!wayFLushedFir && !wayFLushedSec && !(io.write.bits.entry.ftqIdx > io.readFtqIdx)) {  // update readPtr if pred right, otherwise will reduce the space of Waylookup
    //   readPtr := writePtr
    // }
    entries(writePtr.value) := io.write.bits.entry
  }

  XSPerfAccumulate("way_prediction_right", io.write.fire && !way_diff)
  XSPerfAccumulate("way_prediction_wrong", io.write.fire && way_diff)
  XSPerfAccumulate("way_prediction_flushS1", io.wayFlushS1)
  XSPerfAccumulate("way_prediction_flushS0", io.wayFlushS0)

}
