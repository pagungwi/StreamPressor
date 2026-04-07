package szx

import chisel3._
import chiseltest._
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import chisel3.util._

class SZxBlockProcessorHardwareTest extends AnyFlatSpec with ChiselScalatestTester with Matchers {
  
  "SZxBlockProcessor" should "compress constant data correctly" in {
    test(new SZxBlockProcessor(blockSize = 8, dataWidth = 32)) { dut =>
      println("Testing SZxBlockProcessor with constant data")
      
      // Test data: all values are 42.0f
      val testData = Array.fill(8)(42.0f)
      val medianValue = 42.0f
      val errorBound = 0.1f
      val radius = 1.0f
      
      // Convert to UInt representation
      val testDataUInt = testData.map(java.lang.Float.floatToRawIntBits(_).toLong)
      val medianUInt = java.lang.Float.floatToRawIntBits(medianValue).toLong
      val errorBoundUInt = java.lang.Float.floatToRawIntBits(errorBound).toLong
      val radiusUInt = java.lang.Float.floatToRawIntBits(radius).toLong
      
      println(s"Test data: ${testData.mkString(", ")}")
      println(s"Median: $medianValue")
      println(s"Error bound: $errorBound")
      println(s"Radius: $radius")
      
      // Initialize inputs
      dut.io.errorBound.poke(errorBoundUInt.U)
      dut.io.medianValue.poke(medianUInt.U)
      dut.io.radius.poke(radiusUInt.U)
      dut.io.outputReady.poke(true.B)
      
      // Wait for ready
      dut.clock.step()
      dut.io.inputReady.expect(true.B)
      
      // Input data
      for (i <- 0 until 8) {
        dut.io.inputData(i).poke(testDataUInt(i).U)
      }
      dut.io.inputValid.poke(true.B)
      
      // Step clock and monitor progress
      var cycles = 0
      var compressionComplete = false
      var outputSize = 0
      
      // Debug: Monitor state transitions
      var lastState = -1
      
      while (!compressionComplete && cycles < 100) {
        dut.clock.step()
        cycles += 1
        
        // Check if compression is complete
        if (dut.io.outputValid.peek().litToBoolean) {
          compressionComplete = true
          println(s"Compression completed in $cycles cycles")
          
          // Wait for output formatting to complete
          dut.clock.step()
          
          // Read output
          outputSize = dut.io.outputSize.peek().litValue.toInt
          println(s"Compressed size: $outputSize bytes")
          
          // Read first few output bytes (limited by blockSize)
          for (i <- 0 until math.min(10, math.min(outputSize, 8))) {
            val byte = dut.io.outputData(i).peek().litValue.toInt
            println(f"Output byte $i: 0x$byte%02x")
          }
          
          // Verify basic properties
          outputSize should be > 0
          outputSize should be <= 8 * 4  // Should be smaller than uncompressed
        }
        
        // Print state every 10 cycles and monitor state transitions
        val currentState = dut.io.debugState.peek().litValue.toInt
        val processingIndex = dut.io.debugProcessingIndex.peek().litValue.toInt
        val outputFormatted = dut.io.debugOutputFormatted.peek().litToBoolean
        
        if (currentState != lastState) {
          val stateName = currentState match {
            case 0 => "IDLE"
            case 1 => "FIND_STATS"
            case 2 => "CALC_PRECISION"
            case 3 => "COMPRESS"
            case 4 => "OUTPUT"
            case _ => "UNKNOWN"
          }
          println(s"Cycle $cycles: State transition to $stateName, outputFormatted=$outputFormatted")
          
          // Additional debug for OUTPUT state
          if (currentState == 4) { // OUTPUT state
            val reqLength = dut.io.debugReqLength.peek().litValue.toInt
            val residualIndex = dut.io.debugResidualIndex.peek().litValue.toInt
            val outputReg0 = dut.io.debugOutputReg0.peek().litValue.toInt
            println(s"  DEBUG: reqLength=$reqLength, residualIndex=$residualIndex, outputReg[0]=$outputReg0")
          }
          lastState = currentState
        }
        
        if (cycles % 10 == 0) {
          val stateName = currentState match {
            case 0 => "IDLE"
            case 1 => "FIND_STATS"
            case 2 => "CALC_PRECISION"
            case 3 => "COMPRESS"
            case 4 => "OUTPUT"
            case _ => "UNKNOWN"
          }
          println(s"Cycle $cycles: busy=${dut.io.busy.peek().litToBoolean}, state=$stateName, procIdx=$processingIndex")
        }
        
        // Monitor OUTPUT state more frequently
        if (currentState == 4) { // OUTPUT state
          val outputReg0 = dut.io.debugOutputReg0.peek().litValue.toInt
          if (outputReg0 != 0) {
            println(s"  Cycle $cycles: outputReg[0] changed to 0x$outputReg0%02x")
          }
        }
        

      }
      
      compressionComplete shouldBe true
      println(s"Test passed! Compression ratio: ${(8.0 * 4.0) / outputSize}:1")
    }
  }
  
  it should "compress linear data correctly" in {
    test(new SZxBlockProcessor(blockSize = 8, dataWidth = 32)) { dut =>
      println("\nTesting SZxBlockProcessor with linear data")
      
      // Test data: linear sequence
      val testData = (0 until 8).map(_.toFloat).toArray
      val medianValue = 3.5f
      val errorBound = 0.1f
      val radius = 4.0f
      
      // Convert to UInt representation
      val testDataUInt = testData.map(java.lang.Float.floatToRawIntBits(_).toLong)
      val medianUInt = java.lang.Float.floatToRawIntBits(medianValue).toLong
      val errorBoundUInt = java.lang.Float.floatToRawIntBits(errorBound).toLong
      val radiusUInt = java.lang.Float.floatToRawIntBits(radius).toLong
      
      println(s"Test data: ${testData.mkString(", ")}")
      println(s"Median: $medianValue")
      
      // Initialize inputs
      dut.io.errorBound.poke(errorBoundUInt.U)
      dut.io.medianValue.poke(medianUInt.U)
      dut.io.radius.poke(radiusUInt.U)
      dut.io.outputReady.poke(true.B)
      
      // Wait for ready
      dut.clock.step()
      dut.io.inputReady.expect(true.B)
      
      // Input data
      for (i <- 0 until 8) {
        dut.io.inputData(i).poke(testDataUInt(i).U)
      }
      dut.io.inputValid.poke(true.B)
      
      // Step clock until compression complete
      var cycles = 0
      while (!dut.io.outputValid.peek().litToBoolean && cycles < 100) {
        dut.clock.step()
        cycles += 1
      }
      
      dut.io.outputValid.expect(true.B)
      
      // Add one more clock step to ensure output formatting is complete
      dut.clock.step()
      
      val outputSize = dut.io.outputSize.peek().litValue.toInt
      println(s"Linear data compression completed in $cycles cycles")
      println(s"Compressed size: $outputSize bytes")
      println(s"Compression ratio: ${(8.0 * 4.0) / outputSize}:1")
      
      outputSize should be > 0
      outputSize should be <= 8 * 4
    }
  }
  
  it should "handle different error bounds" in {
    test(new SZxBlockProcessor(blockSize = 8, dataWidth = 32)) { dut =>
      println("\nTesting SZxBlockProcessor with different error bounds")
      
      val testData = Array(1.0f, 1.1f, 1.2f, 1.3f, 1.4f, 1.5f, 1.6f, 1.7f)
      val medianValue = 1.35f
      val radius = 0.5f
      
      val testDataUInt = testData.map(java.lang.Float.floatToRawIntBits(_).toLong)
      val medianUInt = java.lang.Float.floatToRawIntBits(medianValue).toLong
      val radiusUInt = java.lang.Float.floatToRawIntBits(radius).toLong
      
      // Test different error bounds
      val errorBounds = List(0.1f, 0.01f, 0.001f)
      
      for (errorBound <- errorBounds) {
        println(s"\nTesting with error bound: $errorBound")
        
        val errorBoundUInt = java.lang.Float.floatToRawIntBits(errorBound).toLong
        
        // Reset and initialize
        dut.reset.poke(true.B)
        dut.clock.step()
        dut.reset.poke(false.B)
        
        dut.io.errorBound.poke(errorBoundUInt.U)
        dut.io.medianValue.poke(medianUInt.U)
        dut.io.radius.poke(radiusUInt.U)
        dut.io.outputReady.poke(true.B)
        
        // Input data
        for (i <- 0 until 8) {
          dut.io.inputData(i).poke(testDataUInt(i).U)
        }
        dut.io.inputValid.poke(true.B)
        
        // Wait for completion
        var cycles = 0
        while (!dut.io.outputValid.peek().litToBoolean && cycles < 100) {
          dut.clock.step()
          cycles += 1
        }
        
        val outputSize = dut.io.outputSize.peek().litValue.toInt
        println(s"  Cycles: $cycles, Size: $outputSize bytes, Ratio: ${(8.0 * 4.0) / outputSize}:1")
        
        outputSize should be > 0
      }
    }
  }
  
  it should "measure performance characteristics" in {
    test(new SZxBlockProcessor(blockSize = 128, dataWidth = 32)) { dut =>
      println("\nPerformance test with 128-element block")
      
      // Generate test data
      val testData = (0 until 128).map(i => (i * 0.1f).toFloat).toArray
      val medianValue = 6.35f
      val errorBound = 0.01f
      val radius = 6.4f
      
      val testDataUInt = testData.map(java.lang.Float.floatToRawIntBits(_).toLong)
      val medianUInt = java.lang.Float.floatToRawIntBits(medianValue).toLong
      val errorBoundUInt = java.lang.Float.floatToRawIntBits(errorBound).toLong
      val radiusUInt = java.lang.Float.floatToRawIntBits(radius).toLong
      
      // Initialize
      dut.io.errorBound.poke(errorBoundUInt.U)
      dut.io.medianValue.poke(medianUInt.U)
      dut.io.radius.poke(radiusUInt.U)
      dut.io.outputReady.poke(true.B)
      
      // Input data
      for (i <- 0 until 128) {
        dut.io.inputData(i).poke(testDataUInt(i).U)
      }
      dut.io.inputValid.poke(true.B)
      
      // Measure cycles
      var cycles = 0
      var busyCycles = 0
      
      while (!dut.io.outputValid.peek().litToBoolean && cycles < 1000) {
        dut.clock.step()
        cycles += 1
        
        if (dut.io.busy.peek().litToBoolean) {
          busyCycles += 1
        }
      }
      
      val outputSize = dut.io.outputSize.peek().litValue.toInt
      val compressionRatio = (128.0 * 4) / outputSize
      
      println(s"Performance Results:")
      println(s"  Total cycles: $cycles")
      println(s"  Busy cycles: $busyCycles")
      println(s"  Compression ratio: $compressionRatio:1")
      println(s"  Throughput: ${128.0 / cycles} elements/cycle")
      println(s"  Efficiency: ${(busyCycles.toDouble / cycles) * 100}%")
      
      cycles should be > 0
      cycles should be < 1000
      outputSize should be > 0
      compressionRatio should be > 1.0
    }
  }
} 