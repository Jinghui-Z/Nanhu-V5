package xiangshan.backend.ctrlblock

import org.chipsalliance.cde.config.Parameters
import chisel3.util._
import chisel3._
import xs.utils.{HasCircularQueuePtrHelper, XORFold, GatedValidRegNext}
import xiangshan.frontend.{FtqRead, PreDecodeInfo, Ftq_RF_Components}
import xiangshan.{MemPredUpdateReq, Redirect, XSBundle, XSModule}
import xiangshan.frontend.FtqPtr

class RedirectGenerator(implicit p: Parameters) extends XSModule
  with HasCircularQueuePtrHelper {

  class RedirectGeneratorIO(implicit p: Parameters) extends XSBundle {
    def numRedirect = backendParams.numRedirect

    val hartId = Input(UInt(8.W))
    val oldestExuRedirect = Flipped(ValidIO(new Redirect))
    val oldestExuOutPredecode = Input(new PreDecodeInfo) // guarded by exuRedirect.valid
    val loadReplay = Flipped(ValidIO(new Redirect))
    val robFlush = Flipped(ValidIO(new Redirect))
    val stage2Redirect = ValidIO(new Redirect)

    val redirectPcReadAddr = Output(new FtqPtr)
    val redirectPcReadOffset = Output(UInt(log2Ceil(PredictWidth).W))
    val redirectPcReadData = Input(UInt(VAddrBits.W))
    val redirectOut = ValidIO(new Redirect)

    val memPredUpdate = Output(new MemPredUpdateReq)
    val memPredPcRead = new FtqRead(UInt(VAddrBits.W)) // read req send form stage 2
    val stage2oldestOH = Output(UInt((1 + 1).W))
  }

  val io = IO(new RedirectGeneratorIO)

  val loadRedirect = io.loadReplay
  val robFlush = io.robFlush
  val allRedirect: Vec[ValidIO[Redirect]] = VecInit(io.oldestExuRedirect, loadRedirect)
  val oldestOneHot = Redirect.selectOldestRedirect(allRedirect)
  val flushAfter = RegInit(0.U.asTypeOf(ValidIO(new Redirect)))
  val needFlushVec = VecInit(allRedirect.map(_.bits.robIdx.needFlush(flushAfter) || robFlush.valid))
  val oldestValid = VecInit(oldestOneHot.zip(needFlushVec).map { case (v, f) => v && !f }).asUInt.orR
  val oldestExuRedirect = io.oldestExuRedirect
  val oldestExuPredecode = io.oldestExuOutPredecode
  val oldestRedirect = Mux1H(oldestOneHot, allRedirect)
  val s0_redirect_bits_reg = oldestRedirect.bits
  val s0_redirect_valid_reg = oldestValid
  val s0_redirect_onehot = oldestOneHot
  val s0_target = oldestRedirect.bits.cfiUpdate.target

  io.redirectPcReadAddr := s0_redirect_bits_reg.ftqIdx
  io.redirectPcReadOffset := s0_redirect_bits_reg.ftqOffset

  if (backendParams.debugEn){
    dontTouch(oldestValid)
    dontTouch(needFlushVec)
  }
  val flushAfterCounter = Reg(UInt(3.W))
  val robFlushOrExuFlushValid = oldestValid || robFlush.valid
  when(robFlushOrExuFlushValid) {
    flushAfter.valid := true.B
    flushAfter.bits := Mux(robFlush.valid, robFlush.bits, oldestRedirect.bits)
  }.elsewhen(!flushAfterCounter(0)) {
    flushAfter.valid := false.B
  }
  when(robFlushOrExuFlushValid) {
    flushAfterCounter := "b111".U
  }.elsewhen(flushAfterCounter(0)){
    flushAfterCounter := flushAfterCounter >> 1
  }
  // stage1 -> stage2

  private val s1_redirect_bits_reg = RegEnable(oldestRedirect.bits, oldestValid)
  private val s1_redirect_valid_reg = GatedValidRegNext(oldestValid)
  private val s1_redirect_onehot = VecInit(oldestOneHot.map(x => GatedValidRegNext(x)))
  private val s1_pcReadReg = RegEnable(io.redirectPcReadData, true.B)
  private val s1_jmpTargetReg = RegEnable(s0_target, s0_redirect_valid_reg)
  private val s1_UopPdReg = RegEnable(oldestExuPredecode, s0_redirect_valid_reg)
  private val s1_robIdxReg = RegEnable(s0_redirect_bits_reg.robIdx, s0_redirect_valid_reg)
  private val snpc = s1_pcReadReg + Mux(s1_UopPdReg.isRVC, 2.U, 4.U)
  private val redirectTarget = WireInit(snpc)
  when(s1_redirect_bits_reg.debugIsMemVio){
    redirectTarget := s1_pcReadReg
  }.otherwise{
    redirectTarget := s1_jmpTargetReg
  }

  io.redirectOut.valid := s1_redirect_valid_reg && !s1_robIdxReg.needFlush(io.oldestExuRedirect) && !s1_robIdxReg.needFlush(io.loadReplay)
  io.redirectOut.bits := s1_redirect_bits_reg
  io.redirectOut.bits.cfiUpdate.pc := s1_pcReadReg
  io.redirectOut.bits.cfiUpdate.pd := s1_UopPdReg
  io.redirectOut.bits.cfiUpdate.target := redirectTarget

  io.stage2Redirect.valid := s1_redirect_valid_reg && !robFlush.valid
  io.stage2Redirect.bits := s1_redirect_bits_reg
  io.stage2Redirect.bits.cfiUpdate.pd := RegEnable(oldestExuPredecode, oldestValid)
  io.stage2oldestOH := s1_redirect_onehot.asUInt

  val s1_isReplay = s1_redirect_onehot.last

  // get pc from ftq
  // valid only if redirect is caused by load violation
  // store_pc is used to update store set
  val store_pc = io.memPredPcRead(s1_redirect_valid_reg, s1_redirect_bits_reg.stFtqIdx, s1_redirect_bits_reg.stFtqOffset)
  val real_pc = s1_pcReadReg
  // update load violation predictor if load violation redirect triggered
  val s2_redirect_bits_reg = RegEnable(s1_redirect_bits_reg, s1_redirect_valid_reg)
  io.memPredUpdate.valid := GatedValidRegNext(s1_isReplay && s1_redirect_valid_reg && s1_redirect_bits_reg.flushItself(), init = false.B)
  // update wait table
  io.memPredUpdate.waddr := RegEnable(XORFold(real_pc(VAddrBits - 1, 1), MemPredPCWidth), s1_isReplay && s1_redirect_valid_reg)
  io.memPredUpdate.wdata := true.B
  // update store set
  io.memPredUpdate.ldpc := RegEnable(XORFold(real_pc(VAddrBits - 1, 1), MemPredPCWidth), s1_isReplay && s1_redirect_valid_reg)
  // store pc is ready 1 cycle after s1_isReplay is judged
  io.memPredUpdate.stpc := RegEnable(XORFold(store_pc(VAddrBits - 1, 1), MemPredPCWidth), s1_isReplay && s1_redirect_valid_reg)

}
