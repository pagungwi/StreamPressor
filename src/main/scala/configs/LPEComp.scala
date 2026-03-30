// See LICENSE.txt in the project root for license information.
// Author: Kazutomo Yoshii <kazutomo@mcs.anl.gov>

package configs

import chisel3._
import chisel3.util._
import _root_.circt.stage.ChiselStage
import lpe.{LPEncoder, LPDecoder}
import common.{V2FConv, F2VConv, ClzParam, MapFP2UInt}

class StreamPressorTop(
  val bw: Int = 32,
  val fpmode: Boolean = true,
  val coefficients: Seq[Int] = Seq(4, -6, 4, -1)
) extends Module {
 
  import lpe.LagrangePredUtil._
  val sint_bw = outSIntBits(bw, coefficients)
  val uint_bw = sint_bw - 1
 
  override def desiredName = s"StreamPressorTop_bw${bw}_fp${fpmode}"
 
  val io = IO(new Bundle {
    // Compression input
    val in_valid    = Input(Bool())
    val in_data     = Input(UInt(bw.W))
 
    // Compressed output (encoder side)
    val enc_valid   = Output(Bool())
    val enc_data    = Output(UInt(uint_bw.W))
    val enc_sign    = Output(UInt(1.W))
 
    // Decompression input
    val dec_in_valid = Input(Bool())
    val dec_in_data  = Input(UInt(uint_bw.W))
    val dec_in_sign  = Input(UInt(1.W))
 
    // Decompressed output
    val dec_valid   = Output(Bool())
    val dec_out     = Output(UInt(bw.W))
  })
 
  // Encoder path
  val encoder = Module(new LPEncoder(bw, fpmode, coefficients))
  encoder.io.in_data := io.in_data
  io.enc_data  := encoder.io.out_data
  io.enc_sign  := encoder.io.out_sign
 
  // Register valid through one pipeline stage (encoder is combinational + 1 reg in LagrangePred)
  io.enc_valid := RegNext(io.in_valid, false.B)
 
  // Decoder path

  val decoder = Module(new LPDecoder(bw, fpmode, coefficients))
  decoder.io.in_data := io.dec_in_data
  decoder.io.in_sign := io.dec_in_sign
  io.dec_out   := decoder.io.out
 
  io.dec_valid := RegNext(io.dec_in_valid, false.B)
}
 
/**
 * VerilogEmitter - generates SystemVerilog for all pipeline modules
 * Run with: sbt 'runMain configs.VerilogEmitter'
 * Output goes to: generated-src/
 */
object VerilogEmitter extends App {
  val outDir = "generated-src"
  val bw = 32
  val coefficients = Seq(4, -6, 4, -1)
 
  def emit(name: String)(gen: => chisel3.RawModule): Unit = {
    println(s"[VerilogEmitter] Generating $name ...")
    ChiselStage.emitSystemVerilogFile(
      gen,
      firtoolOpts = Array(
        "--disable-all-randomization",
        "--strip-debug-info",
        "--lowering-options=disallowLocalVariables,noAlwaysComb,disallowPackedArrays,locationInfoStyle=none"
      ),
      args = Array("--target-dir", outDir)
    )
    println(s"[VerilogEmitter] Done -> $outDir/")
  }
 
  // Individual modules
  emit("ClzParam")       { new ClzParam(64) }
  emit("MapFP2UInt")     { new MapFP2UInt(bw) }
  emit("V2FConv")        { new V2FConv() }
  emit("F2VConv")        { new F2VConv() }
  emit("LPEncoder")      { new LPEncoder(bw, fpmode = true,  coefficients = coefficients) }
  emit("LPDecoder")      { new LPDecoder(bw, fpmode = true,  coefficients = coefficients) }
 
  // Full pipeline top-level
  emit("StreamPressorTop") { new StreamPressorTop(bw, fpmode = true, coefficients = coefficients) }
 
  println(s"\n[VerilogEmitter] All files written to ./$outDir/")
  println(s"[VerilogEmitter] Top-level module for Hammer: StreamPressorTop_bw${bw}_fptrue")
}
