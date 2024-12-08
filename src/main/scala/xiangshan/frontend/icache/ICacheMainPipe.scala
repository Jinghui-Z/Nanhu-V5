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
import difftest._
import freechips.rocketchip.tilelink.ClientStates
import xiangshan._
import xiangshan.cache.mmu._
import utils._
import xs.utils._
import xs.utils.perf._
import xiangshan.backend.fu.{PMPReqBundle, PMPRespBundle}
import xiangshan.frontend.{FtqICacheInfo, FtqToICacheRequestBundle, ExceptionType, BpuFlushInfo, FtqPtr}
import xiangshan.cache.mmu.Pbmt

class ICacheMainPipeReq(implicit p: Parameters) extends ICacheBundle
{
  val vaddr  = UInt(VAddrBits.W)
  def vSetIdx = get_idx(vaddr)
}

class ICacheMainPipeResp(implicit p: Parameters) extends ICacheBundle
{
  val vaddr    = UInt(VAddrBits.W)
  val data     = UInt((blockBits).W)
  val paddr    = UInt(PAddrBits.W)
  val gpaddr    = UInt(GPAddrBits.W)
  val isForVSnonLeafPTE   = Bool()
  val exception = UInt(ExceptionType.width.W)
  val pmp_mmio  = Bool()
  val itlb_pbmt = UInt(Pbmt.width.W)
  val exceptionFromBackend = Bool()
}

class ICacheMainPipeBundle(implicit p: Parameters) extends ICacheBundle
{
  val req  = Flipped(Decoupled(new FtqToICacheRequestBundle))
  val flushFromBpu      = Flipped(new BpuFlushInfo)
  val resp = Vec(PortNumber, ValidIO(new ICacheMainPipeResp))
  val topdownIcacheMiss = Output(Bool())
  val topdownItlbMiss = Output(Bool())
}

class ICacheMetaReqBundle(implicit p: Parameters) extends ICacheBundle{
  val toIMeta       = DecoupledIO(new ICacheReadBundle)
  val fromIMeta     = Input(new ICacheMetaRespBundle)
}

class ICacheDataReqBundle(implicit p: Parameters) extends ICacheBundle{
  val toIData       = Vec(partWayNum, DecoupledIO(new ICacheReadBundle))
  val fromIData     = Input(new ICacheDataRespBundle)
}

class ICacheMSHRBundle(implicit p: Parameters) extends ICacheBundle{
  val req   = Decoupled(new ICacheMissReq)
  val resp  = Flipped(ValidIO(new ICacheMissResp))
}

class ICachePMPBundle(implicit p: Parameters) extends ICacheBundle{
  val req  = Valid(new PMPReqBundle())
  val resp = Input(new PMPRespBundle())
}

class ICachePerfInfo(implicit p: Parameters) extends ICacheBundle{
  val only_0_hit     = Bool()
  val only_0_miss    = Bool()
  val hit_0_hit_1    = Bool()
  val hit_0_miss_1   = Bool()
  val miss_0_hit_1   = Bool()
  val miss_0_miss_1  = Bool()
  val hit_0_except_1 = Bool()
  val miss_0_except_1 = Bool()
  val except_0       = Bool()
  val bank_hit       = Vec(2,Bool())
  val hit            = Bool()
}

class ICacheMainPipeInterface(implicit p: Parameters) extends ICacheBundle {
  val hartId = Input(UInt(hartIdLen.W))
  /*** internal interface ***/
  val dataArray     = new ICacheDataReqBundle
  /** prefetch io */
  val touch = Vec(PortNumber,ValidIO(new ReplacerTouch))
  val wayLookupRead = Flipped(DecoupledIO(new WayLookupInfo))

  val mshr          = new ICacheMSHRBundle
  val errors        = Output(Vec(PortNumber, ValidIO(new L1CacheErrorInfo)))
  /*** outside interface ***/
  //val fetch       = Vec(PortNumber, new ICacheMainPipeBundle)
  /* when ftq.valid is high in T + 1 cycle
   * the ftq component must be valid in T cycle
   */
  val fetch       = new ICacheMainPipeBundle
  val itlb        = Vec(PortNumber, new TlbRequestIO)
  val pmp         = Vec(PortNumber, new ICachePMPBundle)
  val respStall   = Input(Bool())

  val csr_parity_enable = Input(Bool())
  val flush = Input(Bool())
  val wayFlushS0 = Input(Bool())
  val wayFlushS1 = Input(Bool())
  val readFtqIdx = Output(new FtqPtr)
  val icacheS1Ready = Output(Bool())

  val perfInfo = Output(new ICachePerfInfo)
}

class ICacheDB(implicit p: Parameters) extends ICacheBundle {
  val blk_vaddr   = UInt((VAddrBits - blockOffBits).W)
  val blk_paddr   = UInt((PAddrBits - blockOffBits).W)
  val hit         = Bool()
}

class ICacheMainPipe(implicit p: Parameters) extends ICacheModule
{
  val io = IO(new ICacheMainPipeInterface)

  /** Input/Output port */
  val (toITLB,  fromITLB) = (io.itlb.map(_.req), io.itlb.map(_.resp))
  val (fromFtq, toIFU)    = (io.fetch.req,          io.fetch.resp)
  val (toData,  fromData) = (io.dataArray.toIData,  io.dataArray.fromIData)
  val (toMSHR,  fromMSHR) = (io.mshr.req,           io.mshr.resp)
  val (toPMP,   fromPMP)  = (io.pmp.map(_.req),     io.pmp.map(_.resp))
  val fromWayLookup = io.wayLookupRead

  val from_bpu_s0_flush, from_bpu_s1_flush  = WireInit(false.B)
  // Statistics on the frequency distribution of FTQ fire interval
  val cntFtqFireInterval = RegInit(0.U(32.W))
  cntFtqFireInterval := Mux(fromFtq.fire, 1.U, cntFtqFireInterval + 1.U)
  XSPerfHistogram("ftq2icache_fire",
                  cntFtqFireInterval, fromFtq.fire,
                  1, 300, 1, right_strict = true)

  /** pipeline control signal */
  val s1_ready, s2_ready = Wire(Bool())
  val s0_fire,  s1_fire , s2_fire  = Wire(Bool())
  val s0_flush,  s1_flush , s2_flush  = Wire(Bool())

  /**
    ******************************************************************************
    * ICache Stage 0
    * - send req to data SRAM
    * - get waymask and tlb info from wayLookup
    ******************************************************************************
    */

  /** s0 control */
  // 0,1,2,3 -> dataArray(data); 4 -> mainPipe
  // Ftq RegNext Register
  val fromFtqReq          = fromFtq.bits.pcMemRead
  val s0_valid            = fromFtq.valid
  val s0_req_valid_all    = (0 until partWayNum + 1).map(i => fromFtq.bits.readValid(i))
  val s0_req_vaddr_all    = (0 until partWayNum + 1).map(i => VecInit(Seq(fromFtqReq(i).startAddr, fromFtqReq(i).nextlineStart)))
  val s0_req_vSetIdx_all  = (0 until partWayNum + 1).map(i => VecInit(s0_req_vaddr_all(i).map(get_idx)))
  val s0_req_offset_all   = (0 until partWayNum + 1).map(i => s0_req_vaddr_all(i)(0)(log2Ceil(blockBytes)-1, 0))
  val s0_doubleline_all   = (0 until partWayNum + 1).map(i => fromFtq.bits.readValid(i) && fromFtqReq(i).crossCacheline)

  val s0_wayPred          = fromFtq.bits.pcMemRead.last.wayPred
  val s0_waymasksPred     = VecInit(s0_wayPred.map{pred => Mux(pred.valid, UIntToOH(pred.bits), 0.U(nWays.W))}) 
  val s0_req_ftqIdx       = fromFtq.bits.pcMemRead.last.ftqIdx
  val s0_req_vaddr        = s0_req_vaddr_all.last
  val s0_req_vSetIdx      = s0_req_vSetIdx_all.last
  val s0_doubleline       = s0_doubleline_all.last

  val s0_ftq_exception = VecInit((0 until PortNumber).map(i => ExceptionType.fromFtq(fromFtq.bits)))
  val s0_excp_fromBackend = fromFtq.bits.backendIaf || fromFtq.bits.backendIpf || fromFtq.bits.backendIgpf

  /**
    ******************************************************************************
    * get waymask from wayLookup or way prediction
    ******************************************************************************
    */
  val s0_waymasks         = Mux(fromWayLookup.valid, VecInit(fromWayLookup.bits.waymask.map(_.asTypeOf(Vec(nWays, Bool())))),
                                VecInit(s0_waymasksPred.map(_.asTypeOf(Vec(nWays, Bool())))))
  fromWayLookup.ready := s0_fire
  val s0_req_ptags      = fromWayLookup.bits.ptag
  val s0_hits           = VecInit(s0_waymasks.map(_.reduce(_ || _)))
  io.readFtqIdx         := s0_req_ftqIdx

  when(s0_fire && fromWayLookup.valid){
    assert((0 until PortNumber).map(i => (s0_req_vSetIdx(i) === fromWayLookup.bits.vSetIdx(i)) && 
    (s0_req_ftqIdx === fromWayLookup.bits.ftqIdx)).reduce(_&&_),
           "vSetIdxs or ftqIdx from ftq and wayLookup are different! vaddr0=0x%x ftq: vidx0=0x%x vidx1=0x%x wayLookup: vidx0=0x%x vidx1=0x%x",
           s0_req_vaddr(0), s0_req_vSetIdx(0), s0_req_vSetIdx(1), fromWayLookup.bits.vSetIdx(0), fromWayLookup.bits.vSetIdx(1))
  }

  // val s0_exception_out = ExceptionType.merge(
  //   s0_ftq_exception,  // backend-requested exception has the highest priority
  //   s0_itlb_exception
  // )

  /**
    ******************************************************************************
    * data SRAM request
    ******************************************************************************
    */
  for(i <- 0 until partWayNum) {
    toData(i).valid             := s0_req_valid_all(i)
    toData(i).bits.isDoubleLine := s0_doubleline_all(i)
    toData(i).bits.vSetIdx      := s0_req_vSetIdx_all(i)
    toData(i).bits.blkOffset    := s0_req_offset_all(i)
    toData(i).bits.wayMask      := s0_waymasks
  }

  from_bpu_s0_flush := (io.fetch.flushFromBpu.shouldFlushByStage2(s0_req_ftqIdx) ||
                                              io.fetch.flushFromBpu.shouldFlushByStage3(s0_req_ftqIdx))
  val s0_can_go = toData.last.ready && s1_ready && toITLB(0).ready && toITLB(1).ready
  s0_flush  := io.flush || io.wayFlushS0 || from_bpu_s0_flush
  s0_fire   := s0_valid && s0_can_go && !s0_flush

  fromFtq.ready := s0_can_go

  /**
    ******************************************************************************
    * ICache Stage 1
    * - PMP check
    * - get Data SRAM read responses (latched for pipeline stop)
    * - monitor missUint response port
    ******************************************************************************
    */
  val s1_valid = generatePipeControl(lastFire = s0_fire, thisFire = s1_fire, thisFlush = s1_flush, lastFlush = s0_flush)  // lastFlush is way flush or bpu flush

  val s1_req_vaddr        = RegEnable(s0_req_vaddr,        0.U.asTypeOf(s0_req_vaddr),     s0_fire)
  val s1_req_ftqIdx       = RegEnable(s0_req_ftqIdx, 0.U.asTypeOf(s0_req_ftqIdx), s0_fire)
  val s1_doubleline       = RegEnable(s0_doubleline,       0.U.asTypeOf(s0_doubleline),    s0_fire)
  val s1_SRAMhits         = RegEnable(s0_hits,             0.U.asTypeOf(s0_hits),          s0_fire)
  val s1_ftq_exception   = RegEnable(s0_ftq_exception,    0.U.asTypeOf(s0_ftq_exception), s0_fire)
  val s1_excp_fromBackend = RegEnable(s0_excp_fromBackend, false.B,                        s0_fire)
  // val s1_itlb_pbmt        = RegEnable(s0_itlb_pbmt,        0.U.asTypeOf(s0_itlb_pbmt),     s0_fire)
  val s1_waymasks         = RegEnable(s0_waymasks,        0.U.asTypeOf(s0_waymasks),      s0_fire)
  val s1_wayIsPred        = RegEnable(!fromWayLookup.valid, false.B,                      s0_fire)
  // val s1_meta_codes       = RegEnable(s0_meta_codes,       0.U.asTypeOf(s0_meta_codes),    s0_fire)

  val s1_req_vSetIdx  = s1_req_vaddr.map(get_idx)
  // val s1_req_paddr    = s1_req_vaddr.zip(s1_req_ptags).map{case(vaddr, ptag) => get_paddr_from_ptag(vaddr, ptag)}

  val m_idle :: m_itlbResend :: m_waitDeqWay:: m_enterS2 :: Nil = Enum(4)
  val state = RegInit(m_idle)
  val next_state = WireDefault(state)
  dontTouch(state)
  dontTouch(next_state)
  state := next_state
  /**
  ******************************************************************************
  * resend itlb req if miss
  ******************************************************************************
  */
  val s1_wait_itlb  = RegInit(VecInit(Seq.fill(PortNumber)(false.B)))
  (0 until PortNumber).foreach { i =>
    when(s1_flush) {
      s1_wait_itlb(i) := false.B
    }.elsewhen(RegNext(s0_fire) && fromITLB(i).bits.miss) {
      s1_wait_itlb(i) := true.B
    }.elsewhen(s1_wait_itlb(i) && !fromITLB(i).bits.miss) {
      s1_wait_itlb(i) := false.B
    }
  }

  val s1_need_itlb    = VecInit(Seq((RegNext(s0_fire) || s1_wait_itlb(0)) && fromITLB(0).bits.miss,
                                    (RegNext(s0_fire) || s1_wait_itlb(1)) && fromITLB(1).bits.miss && s1_doubleline))
  val tlb_valid_pulse = VecInit(Seq((RegNext(s0_fire) || s1_wait_itlb(0)) && !fromITLB(0).bits.miss,
                                    (RegNext(s0_fire) || s1_wait_itlb(1)) && !fromITLB(1).bits.miss && s1_doubleline))
  val tlb_valid_latch = VecInit((0 until PortNumber).map(i => ValidHoldBypass(tlb_valid_pulse(i), s1_fire, flush=s1_flush)))
  val itlb_finish     = tlb_valid_latch(0) && (!s1_doubleline || tlb_valid_latch(1))

  switch(state) {
    is(m_idle) {
      when(s1_valid) {
        when(!itlb_finish) {
          next_state := m_itlbResend
        }.elsewhen(!s2_ready) {  // itlb_finish 
          next_state := m_enterS2
        }
      }
    }
    is(m_itlbResend) {
      when(itlb_finish) { 
        when(s1_wayIsPred) {
          next_state := m_waitDeqWay
        }.otherwise{
          when(!s2_ready) {
            next_state := m_enterS2
          }.otherwise {
            next_state := m_idle
          }
        }
      }  
    }
    is(m_waitDeqWay) {
       when(!s2_ready) {
          next_state := m_enterS2
        }.otherwise{
          next_state := m_idle
        } 
    }
    is(m_enterS2) {
      when(s2_ready) {
        next_state := m_idle
      }
    }
  }
  when(s1_flush) {
    next_state := m_idle
  }

  for (i <- 0 until PortNumber) {
    toITLB(i).valid             := s1_need_itlb(i) || (s0_valid && (if(i == 0) true.B else s0_doubleline))
    toITLB(i).bits              := DontCare
    toITLB(i).bits.size         := 3.U
    toITLB(i).bits.vaddr        := Mux(s1_need_itlb(i), s1_req_vaddr(i), s0_req_vaddr(i))
    toITLB(i).bits.debug.pc     := Mux(s1_need_itlb(i), s1_req_vaddr(i), s0_req_vaddr(i))
    toITLB(i).bits.cmd          := TlbCmd.exec
    toITLB(i).bits.no_translate := false.B
  }
  fromITLB.foreach(_.ready := true.B)
  io.itlb.foreach(_.req_kill := false.B)

  /**
    ******************************************************************************
    * Receive resp from ITLB
    ******************************************************************************
    */
  val s1_req_paddr_wire     = VecInit(fromITLB.map(_.bits.paddr(0)))
  val s1_req_paddr_reg      = VecInit((0 until PortNumber).map( i =>
    RegEnable(s1_req_paddr_wire(i), 0.U(PAddrBits.W), tlb_valid_pulse(i))
  ))
  val s1_req_paddr          = VecInit((0 until PortNumber).map( i =>
    Mux(tlb_valid_pulse(i), s1_req_paddr_wire(i), s1_req_paddr_reg(i))
  ))
  val s1_req_ptags    = VecInit(s1_req_paddr.map(get_phy_tag))
  val s1_req_gpaddr_tmp     = VecInit((0 until PortNumber).map( i =>
    ResultHoldBypass(valid = tlb_valid_pulse(i), init = 0.U.asTypeOf(fromITLB(i).bits.gpaddr(0)), data = fromITLB(i).bits.gpaddr(0))
  ))
  val s1_req_isForVSnonLeafPTE_tmp    = VecInit((0 until PortNumber).map( i =>
    ResultHoldBypass(valid = tlb_valid_pulse(i), init = 0.U.asTypeOf(fromITLB(i).bits.isForVSnonLeafPTE), data = fromITLB(i).bits.isForVSnonLeafPTE)
  ))
  val s1_itlb_exception     = VecInit((0 until PortNumber).map( i =>
    ResultHoldBypass(valid = tlb_valid_pulse(i), init = 0.U(ExceptionType.width.W), data = ExceptionType.fromTlbResp(fromITLB(i).bits))
  ))
  val s1_itlb_pbmt          = VecInit((0 until PortNumber).map( i =>
    ResultHoldBypass(valid = tlb_valid_pulse(i), init = 0.U.asTypeOf(fromITLB(i).bits.pbmt(0)), data = fromITLB(i).bits.pbmt(0))
  ))
  val s1_itlb_exception_gpf = VecInit(s1_itlb_exception.map(_ === ExceptionType.gpf))
/* Select gpaddr with the first gpf
  */
  val s1_req_gpaddr = PriorityMuxDefault(
    s1_itlb_exception_gpf zip (0 until PortNumber).map(i => s1_req_gpaddr_tmp(i) - (i << blockOffBits).U),
    0.U.asTypeOf(s1_req_gpaddr_tmp(0))
  )
  val s1_req_isForVSnonLeafPTE = PriorityMuxDefault(
    s1_itlb_exception_gpf zip s1_req_isForVSnonLeafPTE_tmp,
    0.U.asTypeOf(s1_req_isForVSnonLeafPTE_tmp(0))
  )

  val s1_req_offset   = s1_req_vaddr(0)(log2Ceil(blockBytes)-1, 0)

  // do metaArray ECC check
  // val s1_meta_corrupt = VecInit((s1_req_ptags zip s1_meta_codes zip s1_waymasks).map{ case ((meta, code), waymask) =>
  //   val hit_num = PopCount(waymask)
  //   // NOTE: if not hit, encodeMetaECC(meta) =/= code can also be true, but we don't care about it
  //   (encodeMetaECC(meta) =/= code && hit_num === 1.U) ||  // hit one way, but parity code does not match, ECC failure
  //     hit_num > 1.U                                       // hit multi way, must be a ECC failure
  // })

  /**
    ******************************************************************************
    * update replacement status register
    ******************************************************************************
    */
  (0 until PortNumber).foreach{ i =>
    io.touch(i).bits.vSetIdx  := s1_req_vSetIdx(i)
    io.touch(i).bits.way      := OHToUInt(s1_waymasks(i))
  }
  io.touch(0).valid := RegNext(s0_fire) && s1_SRAMhits(0) && !io.wayFlushS1  // TODO: when f1_fire touch?
  io.touch(1).valid := RegNext(s0_fire) && s1_SRAMhits(1) && s1_doubleline && !io.wayFlushS1

  /**
    ******************************************************************************
    * PMP check
    ******************************************************************************
    */
  toPMP.zipWithIndex.foreach { case (p, i) =>
    // if itlb has exception, paddr can be invalid, therefore pmp check can be skipped
    p.valid     := s1_valid // && s1_itlb_exception === ExceptionType.none
    p.bits.addr := s1_req_paddr(i)
    p.bits.size := 3.U // TODO
    p.bits.cmd  := TlbCmd.exec
  }
  val s1_pmp_exception = VecInit(fromPMP.map(ExceptionType.fromPMPResp))
  val s1_pmp_mmio      = VecInit(fromPMP.map(_.mmio))

  // also raise af when meta array corrupt is detected, to cancel fetch
  // val s1_meta_exception = VecInit(s1_meta_corrupt.map(ExceptionType.fromECC(io.csr_parity_enable, _)))

  // merge s1 itlb/pmp/meta exceptions, itlb has the highest priority, pmp next, meta lowest
  val s1_exception_out = ExceptionType.merge(
    s1_ftq_exception,
    s1_itlb_exception,
    s1_pmp_exception
    // s1_meta_exception
  )

  // DO NOT merge pmp mmio and itlb pbmt here, we need them to be passed to IFU separately

  /**
    ******************************************************************************
    * select data from MSHR, SRAM
    ******************************************************************************
    */
  val s1_MSHR_match = VecInit((0 until PortNumber).map(i => (s1_req_vSetIdx(i) === fromMSHR.bits.vSetIdx) &&
                                                            (s1_req_ptags(i) === getPhyTagFromBlk(fromMSHR.bits.blkPaddr)) &&
                                                            fromMSHR.valid && !fromMSHR.bits.corrupt))
  val s1_MSHR_hits  = Seq(s1_valid && s1_MSHR_match(0),
                          s1_valid && (s1_MSHR_match(1) && s1_doubleline))
  val s1_MSHR_datas = fromMSHR.bits.data.asTypeOf(Vec(ICacheDataBanks, UInt((blockBits/ICacheDataBanks).W)))

  val s1_hits = (0 until PortNumber).map(i => ValidHoldBypass(s1_MSHR_hits(i), s1_fire || s1_flush) || s1_SRAMhits(i))

  val s1_bankIdxLow  = s1_req_offset >> log2Ceil(blockBytes/ICacheDataBanks)
  val s1_bankMSHRHit = VecInit((0 until ICacheDataBanks).map(i => (i.U >= s1_bankIdxLow) && s1_MSHR_hits(0) ||
                                                      (i.U < s1_bankIdxLow) && s1_MSHR_hits(1)))
  val s1_datas       = VecInit((0 until ICacheDataBanks).map(i => DataHoldBypass(Mux(s1_bankMSHRHit(i), s1_MSHR_datas(i), fromData.datas(i)),
                                                          s1_bankMSHRHit(i) || RegNext(s0_fire))))
  val s1_codes       = DataHoldBypass(fromData.codes, RegNext(s0_fire))

  from_bpu_s1_flush := s1_valid && io.fetch.flushFromBpu.shouldFlushByStage3(s1_req_ftqIdx)
  s1_flush := io.flush || io.wayFlushS1 || from_bpu_s1_flush
  s1_ready := (s2_ready || !s1_valid) && (next_state === m_idle)
  s1_fire  := s1_valid && s2_ready && !s1_flush && (next_state === m_idle)
  io.icacheS1Ready := s2_ready && (next_state === m_idle)

  /**
    ******************************************************************************
    * ICache Stage 2
    * - send request to MSHR if ICache miss
    * - monitor missUint response port
    * - response to IFU
    ******************************************************************************
    */

  val s2_valid = generatePipeControl(lastFire = s1_fire, thisFire = s2_fire, thisFlush = s2_flush, lastFlush = s1_flush)

  val s2_req_vaddr        = RegEnable(s1_req_vaddr,        0.U.asTypeOf(s1_req_vaddr),     s1_fire)
  val s2_req_ptags        = RegEnable(s1_req_ptags,        0.U.asTypeOf(s1_req_ptags),     s1_fire)
  val s2_req_gpaddr       = RegEnable(s1_req_gpaddr,       0.U.asTypeOf(s1_req_gpaddr),    s1_fire)
  val s2_req_isForVSnonLeafPTE      = RegEnable(s1_req_isForVSnonLeafPTE,      0.U.asTypeOf(s1_req_isForVSnonLeafPTE),   s1_fire)
  val s2_doubleline       = RegEnable(s1_doubleline,       0.U.asTypeOf(s1_doubleline),    s1_fire)
  val s2_exception        = RegEnable(s1_exception_out,    0.U.asTypeOf(s1_exception_out), s1_fire)  // includes itlb/pmp/meta exception
  val s2_excp_fromBackend = RegEnable(s1_excp_fromBackend, false.B,                        s1_fire)
  val s2_pmp_mmio         = RegEnable(s1_pmp_mmio,         0.U.asTypeOf(s1_pmp_mmio),      s1_fire)
  val s2_itlb_pbmt        = RegNext(s1_itlb_pbmt)

  val s2_req_vSetIdx  = s2_req_vaddr.map(get_idx)
  val s2_req_offset   = s2_req_vaddr(0)(log2Ceil(blockBytes)-1, 0)
  val s2_req_paddr    = s2_req_vaddr.zip(s2_req_ptags).map{case(vaddr, ptag) => get_paddr_from_ptag(vaddr, ptag)}

  val s2_SRAMhits     = RegEnable(s1_SRAMhits, 0.U.asTypeOf(s1_SRAMhits), s1_fire)
  val s2_codes        = RegEnable(s1_codes, 0.U.asTypeOf(s1_codes), s1_fire)
  val s2_hits         = RegInit(VecInit(Seq.fill(PortNumber)(false.B)))
  val s2_datas        = RegInit(VecInit(Seq.fill(ICacheDataBanks)(0.U((blockBits/ICacheDataBanks).W))))

  /**
    ******************************************************************************
    * report data parity error
    ******************************************************************************
    */
  // check data error
  val s2_bankSel     = getBankSel(s2_req_offset, s2_valid)
  val s2_bank_corrupt = (0 until ICacheDataBanks).map(i => (encodeDataECC(s2_datas(i)) =/= s2_codes(i)))
  val s2_data_corrupt = (0 until PortNumber).map(port => (0 until ICacheDataBanks).map(bank =>
                         s2_bank_corrupt(bank) && s2_bankSel(port)(bank).asBool).reduce(_||_) && s2_SRAMhits(port))
  // meta error is checked in prefetch pipeline
  // val s2_meta_corrupt = RegEnable(s1_meta_corrupt, 0.U.asTypeOf(s1_meta_corrupt), s1_fire)
  // send errors to top
  (0 until PortNumber).map{ i =>
    // io.errors(i).valid              := io.csr_parity_enable && RegNext(s1_fire) && (s2_meta_corrupt(i) || s2_data_corrupt(i))
    // io.errors(i).bits.report_to_beu := io.csr_parity_enable && RegNext(s1_fire) && (s2_meta_corrupt(i) || s2_data_corrupt(i))
    io.errors(i).valid              := io.csr_parity_enable && RegNext(s1_fire) && s2_data_corrupt(i)
    io.errors(i).bits.report_to_beu := io.csr_parity_enable && RegNext(s1_fire) && s2_data_corrupt(i)
    io.errors(i).bits.paddr         := s2_req_paddr(i)
    io.errors(i).bits.source        := DontCare
    // io.errors(i).bits.source.tag    := s2_meta_corrupt(i)
    io.errors(i).bits.source.tag    := DontCare
    io.errors(i).bits.source.data   := s2_data_corrupt(i)
    io.errors(i).bits.source.l2     := false.B
    io.errors(i).bits.opType        := DontCare
    io.errors(i).bits.opType.fetch  := true.B
  }

  /**
    ******************************************************************************
    * monitor missUint response port
    ******************************************************************************
    */
  val s2_MSHR_match = VecInit((0 until PortNumber).map( i =>
    (s2_req_vSetIdx(i) === fromMSHR.bits.vSetIdx) &&
    (s2_req_ptags(i) === getPhyTagFromBlk(fromMSHR.bits.blkPaddr)) &&
    fromMSHR.valid  // we don't care about whether it's corrupt here
  ))
  val s2_MSHR_hits  = Seq(s2_valid && s2_MSHR_match(0),
                          s2_valid && s2_MSHR_match(1) && s2_doubleline)
  val s2_MSHR_datas = fromMSHR.bits.data.asTypeOf(Vec(ICacheDataBanks, UInt((blockBits/ICacheDataBanks).W)))

  val s2_bankIdxLow  = s2_req_offset >> log2Ceil(blockBytes/ICacheDataBanks)
  val s2_bankMSHRHit = VecInit((0 until ICacheDataBanks).map( i =>
    ((i.U >= s2_bankIdxLow) && s2_MSHR_hits(0)) || ((i.U < s2_bankIdxLow) && s2_MSHR_hits(1))
  ))

  (0 until ICacheDataBanks).foreach{ i =>
    when(s1_fire) {
      s2_datas := s1_datas
    }.elsewhen(s2_bankMSHRHit(i) && !fromMSHR.bits.corrupt) {
      // if corrupt, no need to update s2_datas (it's wrong anyway), to save power
      s2_datas(i) := s2_MSHR_datas(i)
    }
  }

  (0 until PortNumber).foreach{ i =>
    when(s1_fire) {
      s2_hits := s1_hits
    }.elsewhen(s2_MSHR_hits(i)) {
      // update s2_hits even if it's corrupt, to let s2_fire
      s2_hits(i) := true.B
    }
  }

  val s2_l2_corrupt = RegInit(VecInit(Seq.fill(PortNumber)(false.B)))
  (0 until PortNumber).foreach{ i =>
    when(s1_fire) {
      s2_l2_corrupt(i) := false.B
    }.elsewhen(s2_MSHR_hits(i)) {
      s2_l2_corrupt(i) := fromMSHR.bits.corrupt
    }
  }

  /**
    ******************************************************************************
    * send request to MSHR if ICache miss
    ******************************************************************************
    */

  // merge pmp mmio and itlb pbmt
  val s2_mmio = VecInit((s2_pmp_mmio zip s2_itlb_pbmt).map{ case (mmio, pbmt) =>
    mmio || Pbmt.isUncache(pbmt)
  })

  /* s2_exception includes itlb pf/gpf/af, pmp af and meta corruption (af), neither of which should be fetched
   * mmio should not be fetched, it will be fetched by IFU mmio fsm
   * also, if previous has exception, latter port should also not be fetched
   */
  val s2_miss = VecInit((0 until PortNumber).map { i =>
    !s2_hits(i) && (if (i==0) true.B else s2_doubleline) &&
      s2_exception.take(i+1).map(_ === ExceptionType.none).reduce(_&&_) &&
      s2_mmio.take(i+1).map(!_).reduce(_&&_)
  })

  val toMSHRArbiter = Module(new Arbiter(new ICacheMissReq, PortNumber))

  // To avoid sending duplicate requests.
  val has_send = RegInit(VecInit(Seq.fill(PortNumber)(false.B)))
  (0 until PortNumber).foreach{ i =>
    when(s1_fire) {
      has_send(i) := false.B
    }.elsewhen(toMSHRArbiter.io.in(i).fire) {
      has_send(i) := true.B
    }
  }

  (0 until PortNumber).map{ i =>
    toMSHRArbiter.io.in(i).valid          := s2_valid && s2_miss(i) && !has_send(i) && !s2_flush
    toMSHRArbiter.io.in(i).bits.blkPaddr  := getBlkAddr(s2_req_paddr(i))
    toMSHRArbiter.io.in(i).bits.vSetIdx   := s2_req_vSetIdx(i)
  }
  toMSHR <> toMSHRArbiter.io.out

  XSPerfAccumulate("to_missUnit_stall",  toMSHR.valid && !toMSHR.ready)

  val s2_fetch_finish = !s2_miss.reduce(_||_)

  // also raise af if data/l2 corrupt is detected
  val s2_data_exception = VecInit(s2_data_corrupt.map(ExceptionType.fromECC(io.csr_parity_enable, _)))
  val s2_l2_exception   = VecInit(s2_l2_corrupt.map(ExceptionType.fromECC(true.B, _)))

  // merge s2 exceptions, itlb has the highest priority, meta next, meta/data/l2 lowest (and we dont care about prioritizing between this three)
  val s2_exception_out = ExceptionType.merge(
    s2_exception,  // includes itlb/pmp/meta exception
    s2_data_exception,
    s2_l2_exception
  )

  /**
    ******************************************************************************
    * response to IFU
    ******************************************************************************
    */
  (0 until PortNumber).foreach{ i =>
    if(i == 0) {
      toIFU(i).valid          := s2_fire
      toIFU(i).bits.exception := s2_exception_out(i)
      toIFU(i).bits.pmp_mmio  := s2_pmp_mmio(i)   // pass pmp_mmio instead of merged mmio to IFU
      toIFU(i).bits.itlb_pbmt := s2_itlb_pbmt(i)
      toIFU(i).bits.data      := s2_datas.asTypeOf(UInt(blockBits.W))
    } else {
      toIFU(i).valid          := s2_fire && s2_doubleline
      toIFU(i).bits.exception := Mux(s2_doubleline, s2_exception_out(i), ExceptionType.none)
      toIFU(i).bits.pmp_mmio  := s2_pmp_mmio(i) && s2_doubleline
      toIFU(i).bits.itlb_pbmt := Mux(s2_doubleline, s2_itlb_pbmt(i), Pbmt.pma)
      toIFU(i).bits.data      := DontCare
    }
    toIFU(i).bits.exceptionFromBackend := s2_excp_fromBackend
    toIFU(i).bits.vaddr       := s2_req_vaddr(i)
    toIFU(i).bits.paddr       := s2_req_paddr(i)
    toIFU(i).bits.gpaddr      := s2_req_gpaddr  // Note: toIFU(1).bits.gpaddr is actually DontCare in current design
    toIFU(i).bits.isForVSnonLeafPTE     := s2_req_isForVSnonLeafPTE
  }

  s2_flush := io.flush
  s2_ready := (s2_fetch_finish && !io.respStall) || !s2_valid
  s2_fire  := s2_valid && s2_fetch_finish && !io.respStall && !s2_flush

  /**
    ******************************************************************************
    * report Tilelink corrupt error
    ******************************************************************************
    */
  (0 until PortNumber).map{ i =>
    when(RegNext(s2_fire && s2_l2_corrupt(i))){
      io.errors(i).valid                 := true.B
      io.errors(i).bits.report_to_beu    := false.B // l2 should have report that to bus error unit, no need to do it again
      io.errors(i).bits.paddr            := RegNext(s2_req_paddr(i))
      io.errors(i).bits.source.tag       := false.B
      io.errors(i).bits.source.data      := false.B
      io.errors(i).bits.source.l2        := true.B
    }
  }

  /**
    ******************************************************************************
    * performance info. TODO: need to simplify the logic
    ***********************************************************s*******************
    */
  io.perfInfo.only_0_hit      :=  s2_hits(0) && !s2_doubleline
  io.perfInfo.only_0_miss     := !s2_hits(0) && !s2_doubleline
  io.perfInfo.hit_0_hit_1     :=  s2_hits(0) &&  s2_hits(1) && s2_doubleline
  io.perfInfo.hit_0_miss_1    :=  s2_hits(0) && !s2_hits(1) && s2_doubleline
  io.perfInfo.miss_0_hit_1    := !s2_hits(0) &&  s2_hits(1) && s2_doubleline
  io.perfInfo.miss_0_miss_1   := !s2_hits(0) && !s2_hits(1) && s2_doubleline
  io.perfInfo.hit_0_except_1  :=  s2_hits(0) && (s2_exception(1) =/= ExceptionType.none) && s2_doubleline
  io.perfInfo.miss_0_except_1 := !s2_hits(0) && (s2_exception(1) =/= ExceptionType.none) && s2_doubleline
  io.perfInfo.bank_hit(0)     :=  s2_hits(0)
  io.perfInfo.bank_hit(1)     :=  s2_hits(1) && s2_doubleline
  io.perfInfo.except_0        :=  s2_exception(0) =/= ExceptionType.none
  io.perfInfo.hit             :=  s2_hits(0) && (!s2_doubleline || s2_hits(1))

  /** <PERF> fetch bubble generated by icache miss */
  XSPerfAccumulate("icache_bubble_s2_miss", s2_valid && !s2_fetch_finish )
  XSPerfAccumulate("icache_bubble_s0_wayLookup", s0_valid && !fromWayLookup.ready)

  io.fetch.topdownIcacheMiss := !s2_fetch_finish
  io.fetch.topdownItlbMiss := s0_valid && !fromWayLookup.ready

  // class ICacheTouchDB(implicit p: Parameters) extends ICacheBundle{
  //   val blkPaddr  = UInt((PAddrBits - blockOffBits).W)
  //   val vSetIdx   = UInt(idxBits.W)
  //   val waymask   = UInt(log2Ceil(nWays).W)
  // }

  // val isWriteICacheTouchTable = WireInit(Constantin.createRecord("isWriteICacheTouchTable" + p(XSCoreParamsKey).HartId.toString))
  // val ICacheTouchTable = ChiselDB.createTable("ICacheTouchTable" + p(XSCoreParamsKey).HartId.toString, new ICacheTouchDB)

  // val ICacheTouchDumpData = Wire(Vec(PortNumber, new ICacheTouchDB))
  // (0 until PortNumber).foreach{ i =>
  //   ICacheTouchDumpData(i).blkPaddr  := getBlkAddr(s2_req_paddr(i))
  //   ICacheTouchDumpData(i).vSetIdx   := s2_req_vSetIdx(i)
  //   ICacheTouchDumpData(i).waymask   := OHToUInt(s2_tag_match_vec(i))
  //   ICacheTouchTable.log(
  //     data  = ICacheTouchDumpData(i),
  //     en    = io.touch(i).valid,
  //     site  = "req_" + i.toString,
  //     clock = clock,
  //     reset = reset
  //   )
  // }

  /**
    ******************************************************************************
    * difftest refill check
    ******************************************************************************
    */
  if (env.EnableDifftest) {
    val discards = (0 until PortNumber).map { i =>
      val discard = toIFU(i).bits.exception =/= ExceptionType.none || toIFU(i).bits.pmp_mmio ||
        Pbmt.isUncache(toIFU(i).bits.itlb_pbmt)
      discard
    }
    val blkPaddrAll = s2_req_paddr.map(addr => addr(PAddrBits - 1, blockOffBits) << blockOffBits)
    (0 until ICacheDataBanks).map { i =>
      val diffMainPipeOut = DifftestModule(new DiffRefillEvent, dontCare = true)
      diffMainPipeOut.coreid := io.hartId
      diffMainPipeOut.index := (3 + i).U

      val bankSel = getBankSel(s2_req_offset, s2_valid).reduce(_|_)
      val lineSel = getLineSel(s2_req_offset)

      diffMainPipeOut.valid := s2_fire && bankSel(i).asBool && Mux(lineSel(i), !discards(1), !discards(0))
      diffMainPipeOut.addr  := Mux(lineSel(i), blkPaddrAll(1) + (i.U << (log2Ceil(blockBytes/ICacheDataBanks))),
                                               blkPaddrAll(0) + (i.U << (log2Ceil(blockBytes/ICacheDataBanks))))

      diffMainPipeOut.data :=  s2_datas(i).asTypeOf(diffMainPipeOut.data)
      diffMainPipeOut.idtfr := DontCare
    }
  }
}