package szx

import chisel3._
import chiseltest._
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class SZxDecoupledBlockProcessorTest extends AnyFlatSpec with ChiselScalatestTester with Matchers {
  
  "SZxDecoupledBlockProcessor" should "compress constant data correctly" in {
    test(new SZxDecoupledBlockProcessor(blockSize = 8, dataWidth = 32, fifoDepth = 4)) { dut =>
      // Test with constant data (all values = 42)
      val inputBlock = Seq.fill(8)(42)
      val inputVec = inputBlock.map(_.U(32.W))
      
      // Set parameters
      val median = 42.U(32.W)
      val errorBound = 1.U(32.W)
      val radius = 0.U(32.W)
      
      // Create input data bundle
      val inputData = new BlockData(8, 32)
      inputData.data.zip(inputVec).foreach { case (port, value) => port.poke(value) }
      inputData.errorBound.poke(errorBound)
      inputData.medianValue.poke(median)
      inputData.radius.poke(radius)
      
      // Send input
      dut.io.input.valid.poke(true.B)
      dut.io.input.bits.poke(inputData)
      dut.io.output.ready.poke(true.B)
      
      // Wait for processing to start
      dut.clock.step(1)
      dut.io.input.valid.poke(false.B)
      
      // Wait for output
      var cycles = 0
      while (!dut.io.output.valid.peek().litToBoolean && cycles < 100) {
        dut.clock.step(1)
        cycles += 1
      }
      
      dut.io.output.valid.expect(true.B)
      println(s"Output valid after $cycles cycles")
      
      // Check output
      val outSize = dut.io.output.bits.size.peek().litValue
      println(s"Compressed output size: $outSize bytes")
      
      // Print first few bytes
      println("Compressed output bytes:")
      for (i <- 0 until math.min(outSize.toInt, 16)) {
        val byte = dut.io.output.bits.data(i).peek().litValue
        print(f"$byte%02x ")
      }
      println()
      
      // Verify basic properties
      outSize should be > 0
      outSize should be <= (8 * 4) // Max compressed size
    }
  }
  
  "SZxDecoupledBlockProcessor" should "handle multiple blocks with FIFO buffering" in {
    test(new SZxDecoupledBlockProcessor(blockSize = 8, dataWidth = 32, fifoDepth = 4)) { dut =>
      dut.io.output.ready.poke(true.B)
      
      // Send multiple blocks quickly
      for (blockIdx <- 0 until 3) {
        val inputBlock = Seq.fill(8)(42 + blockIdx)
        val inputVec = inputBlock.map(_.U(32.W))
        
        val inputData = new BlockData(8, 32)
        inputData.data.zip(inputVec).foreach { case (port, value) => port.poke(value) }
        inputData.errorBound.poke(1.U(32.W))
        inputData.medianValue.poke((42 + blockIdx).U(32.W))
        inputData.radius.poke(0.U(32.W))
        
        // Send block
        dut.io.input.valid.poke(true.B)
        dut.io.input.bits.poke(inputData)
        dut.clock.step(1)
        dut.io.input.valid.poke(false.B)
        
        println(s"Sent block $blockIdx")
      }
      
      // Collect outputs
      var outputsReceived = 0
      var totalCycles = 0
      
      while (outputsReceived < 3 && totalCycles < 200) {
        dut.clock.step(1)
        totalCycles += 1
        
        if (dut.io.output.valid.peek().litToBoolean) {
          val outSize = dut.io.output.bits.size.peek().litValue
          println(s"Received output $outputsReceived, size: $outSize bytes")
          outputsReceived += 1
        }
      }
      
      outputsReceived should be(3)
      println(s"Processed all blocks in $totalCycles cycles")
    }
  }
  
  "SZxDecoupledBlockProcessor" should "demonstrate latency hiding with FIFOs" in {
    test(new SZxDecoupledBlockProcessor(blockSize = 8, dataWidth = 32, fifoDepth = 4)) { dut =>
      // Test with slow consumer to see FIFO buffering in action
      dut.io.output.ready.poke(false.B) // Slow consumer
      
      // Send blocks quickly
      for (blockIdx <- 0 until 5) {
        val inputBlock = Seq.fill(8)(42 + blockIdx)
        val inputVec = inputBlock.map(_.U(32.W))
        
        val inputData = new BlockData(8, 32)
        inputData.data.zip(inputVec).foreach { case (port, value) => port.poke(value) }
        inputData.errorBound.poke(1.U(32.W))
        inputData.medianValue.poke((42 + blockIdx).U(32.W))
        inputData.radius.poke(0.U(32.W))
        
        dut.io.input.valid.poke(true.B)
        dut.io.input.bits.poke(inputData)
        dut.clock.step(1)
        dut.io.input.valid.poke(false.B)
        
        println(s"Sent block $blockIdx")
      }
      
      // Now enable consumer and see how many outputs are immediately available
      dut.io.output.ready.poke(true.B)
      dut.clock.step(1)
      
      var immediatelyAvailable = 0
      while (dut.io.output.valid.peek().litToBoolean) {
        immediatelyAvailable += 1
        dut.clock.step(1)
      }
      
      println(s"Immediately available outputs: $immediatelyAvailable")
      immediatelyAvailable should be > 0 // Should have some outputs ready due to FIFO buffering
    }
  }
  
  "SZxDecoupledBlockProcessor" should "compress linear data correctly" in {
    test(new SZxDecoupledBlockProcessor(blockSize = 8, dataWidth = 32, fifoDepth = 4)) { dut =>
      // Test with linear data (0, 1, 2, 3, 4, 5, 6, 7)
      val inputBlock = Seq(0, 1, 2, 3, 4, 5, 6, 7)
      val inputVec = inputBlock.map(_.U(32.W))
      
      val inputData = new BlockData(8, 32)
      inputData.data.zip(inputVec).foreach { case (port, value) => port.poke(value) }
      inputData.errorBound.poke(1.U(32.W))
      inputData.medianValue.poke(3.U(32.W)) // Median of 0-7
      inputData.radius.poke(4.U(32.W))
      
      dut.io.input.valid.poke(true.B)
      dut.io.input.bits.poke(inputData)
      dut.io.output.ready.poke(true.B)
      
      dut.clock.step(1)
      dut.io.input.valid.poke(false.B)
      
      var cycles = 0
      while (!dut.io.output.valid.peek().litToBoolean && cycles < 100) {
        dut.clock.step(1)
        cycles += 1
      }
      
      dut.io.output.valid.expect(true.B)
      
      val outSize = dut.io.output.bits.size.peek().litValue
      println(s"Linear data compressed to $outSize bytes")
      
      // Verify compression ratio is reasonable
      val originalSize = 8 * 4 // 8 elements * 4 bytes each
      val compressionRatio = outSize.toDouble / originalSize
      println(f"Compression ratio: $compressionRatio%.3f")
      
      compressionRatio should be < 1.0 // Should compress
    }
  }
}