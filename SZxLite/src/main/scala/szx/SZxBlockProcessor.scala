package szx

import chisel3._
import chisel3.util._

class SZxBlockProcessor(val blockSize: Int = 64, val dataWidth: Int = 32, val parallelWidth: Int = 8) extends Module {
  val io = IO(new Bundle {
    val inputData = Input(Vec(blockSize, UInt(dataWidth.W)))
    val inputValid = Input(Bool())
    val inputReady = Output(Bool())

    val errorBound = Input(UInt(32.W))  // Float32 error bound
    val medianValue = Input(UInt(32.W)) // Pre-computed median
    val radius = Input(UInt(32.W))      // Pre-computed radius

    val outputData = Output(Vec(blockSize * 4, UInt(8.W)))  // Compressed bytes
    val outputValid = Output(Bool())
    val outputReady = Input(Bool())
    val outputSize = Output(UInt(16.W))  // Actual compressed size

    val busy = Output(Bool())

    // Debug outputs
    val debugState = Output(UInt(3.W))
    val debugProcessingIndex = Output(UInt(log2Ceil(blockSize).W))
    val debugReqLength = Output(UInt(8.W))
    val debugResidualIndex = Output(UInt(log2Ceil(blockSize * 4).W))
    val debugOutputReg0 = Output(UInt(8.W))
    val debugOutputFormatted = Output(Bool())
    val debugOutputFormattedCount = Output(UInt(8.W))
  })

  // State machine
  val sIdle :: sFindStats :: sCalculatePrecision :: sCompress :: sOutput :: Nil = Enum(5)

  val state = RegInit(sIdle)
  val busy = RegInit(false.B)

  // Timeout counter to prevent hanging
  val timeoutCounter = RegInit(0.U(16.W))

  // Data registers
  val inputReg = Reg(Vec(blockSize, UInt(dataWidth.W)))
  val outputReg = Reg(Vec(blockSize * 4, UInt(8.W)))  // Larger to hold full compressed output
  val outputSizeReg = RegInit(0.U(16.W))

  // Processing registers
  val processingIndex = RegInit(0.U((log2Ceil(blockSize) + 1).W))  // +1 to handle values up to blockSize
  val minVal = RegInit(0.U(dataWidth.W))
  val maxVal = RegInit(0.U(dataWidth.W))
  val range = RegInit(0.U(dataWidth.W))

  // SZx compression registers
  val reqLength = RegInit(0.U(8.W))
  val reqBytesLength = RegInit(0.U(4.W))
  val rightShiftBits = RegInit(0.U(4.W))
  val prevValue = RegInit(0.U(dataWidth.W))

  // Leading number and residual storage
  val leadingNumbers = RegInit(VecInit(Seq.fill(blockSize)(0.U(2.W))))
  val residualBytes = RegInit(VecInit(Seq.fill(blockSize * 4)(0.U(8.W))))
  val residualIndex = RegInit(0.U(log2Ceil(blockSize * 4).W))

  // Output formatting control
  val outputFormatted = RegInit(false.B)

  // Parallel processing registers
  val parallelProcessing = RegInit(false.B)
  val parallelIndex = RegInit(0.U((log2Ceil(blockSize) + 1).W))
  val parallelResults = RegInit(VecInit(Seq.fill(parallelWidth)(0.U(dataWidth.W))))  // Increased from 4 to 8 **NEW** made this, and others like it, dynamic insead of 8
  val parallelLeadingNums = RegInit(VecInit(Seq.fill(parallelWidth)(0.U(2.W))))     // Increased from 4 to 8
  val parallelResiduals = RegInit(VecInit(Seq.fill(parallelWidth*4)(0.U(8.W))))      // Increased from 16 to 32 (8 elements * 4 bytes max)
  val parallelResidualIndex = RegInit(0.U(6.W))

  // Prefetch buffer (disabled for now)
  // val prefetchBuffer = RegInit(VecInit(Seq.fill(blockSize)(0.U(dataWidth.W))))
  // val prefetchValid = RegInit(false.B)

  // Output signals
  io.inputReady := (state === sIdle) && !busy
  io.outputValid := (state === sOutput)
  io.outputData := outputReg
  io.outputSize := outputSizeReg
  io.busy := busy

  // Debug output for block processor state
  when(io.inputValid) {
    printf("SZxBlockProcessor: inputValid asserted, state=%d, busy=%d, inputReady=%d\n",
           state, busy, io.inputReady)
    printf("SZxBlockProcessor: Received input data[0]=0x%x, data[1]=0x%x, data[2]=0x%x\n",
           io.inputData(0), io.inputData(1), io.inputData(2))
  }

  // Memory stall monitoring (disabled for now)
  // when(state === sCompress) {
  //   memoryStallCycles := memoryStallCycles + 1.U
  //   when(memoryStallCycles % 1000.U === 0.U) {
  //     printf("SZxBlockProcessor: Memory stall cycles: %d\n", memoryStallCycles)
  //   }
  // }

  // Debug output for block processor state every few cycles
  val debugCounter = RegInit(0.U(16.W))  // Increased to 16 bits for more cycles
  debugCounter := debugCounter + 1.U
  when(debugCounter === 0.U) {
    printf("SZxBlockProcessor: Current state=%d, busy=%d, inputReady=%d\n",
           state, busy, io.inputReady)
  }

  // Cycle counting for performance analysis
  val compressionStartCycle = RegInit(0.U(16.W))
  val compressionEndCycle = RegInit(0.U(16.W))

  // Timeout logic - reset if stuck for too long
  when(busy) {
    timeoutCounter := timeoutCounter + 1.U
    when(timeoutCounter > 10000.U) {  // Increased timeout to allow processing 64 elements
      printf("SZxBlockProcessor: Timeout - resetting to idle state\n")
      state := sIdle
      busy := false.B
      timeoutCounter := 0.U
    }
  }.otherwise {
    timeoutCounter := 0.U
  }

  // Memory access counters (disabled for now)
  // val memoryReads = RegInit(0.U(32.W))
  // val memoryWrites = RegInit(0.U(32.W))
  // val memoryAccessCycles = RegInit(0.U(32.W))

  // Memory bandwidth monitoring (disabled for now)
  // val memoryStallCycles = RegInit(0.U(32.W))

  // Debug outputs
  io.debugState := state
  io.debugProcessingIndex := processingIndex
  io.debugReqLength := reqLength
  io.debugResidualIndex := residualIndex
  io.debugOutputReg0 := outputReg(0)

  // Additional debug outputs
  val debugOutputFormatted = RegInit(false.B)
  val debugOutputFormattedCount = RegInit(0.U(8.W))
  debugOutputFormatted := outputFormatted
  debugOutputFormattedCount := debugOutputFormattedCount + Mux(outputFormatted && !debugOutputFormatted, 1.U, 0.U)

  io.debugOutputFormatted := outputFormatted
  io.debugOutputFormattedCount := debugOutputFormattedCount

  // Helper function to get exponent from float32 (matches C implementation)
  def getExponentFloat(value: UInt): UInt = {
    (value(30, 23) - 127.U(8.W)).asUInt
  }

  // Helper function to get precision requirement length (matches C getPrecisionReqLength_double)
  def getPrecisionReqLength(errorBound: UInt): UInt = {
    // For float32, extract exponent and subtract bias
    (errorBound(30, 23) - 127.U(8.W)).asUInt
  }

  // Helper function to detect leading zeros
  def detectLeadingZeros(value: UInt): UInt = {
    MuxCase(0.U(2.W), Seq(
      (value(31, 8) === 0.U) -> 3.U(2.W),
      (value(31, 16) === 0.U) -> 2.U(2.W),
      (value(31, 24) === 0.U) -> 1.U(2.W)
    ))
  }

      // Simple parallel processing - just process 8 elements at once without complex residual tracking
  def processParallelElements(startIdx: UInt): Unit = {
    // Process 8 elements in parallel (debug output removed for performance)
    for (i <- 0 until parallelWidth) {
      val elementIdx = startIdx + i.U
      when(elementIdx < blockSize.U) {
        // For sFindStats: find min/max
        when(state === sFindStats) {
          when(inputReg(elementIdx) < minVal) {
            minVal := inputReg(elementIdx)
          }
          when(inputReg(elementIdx) > maxVal) {
            maxVal := inputReg(elementIdx)
          }
        }

        // For sCompress: compress elements
        when(state === sCompress) {
          val currentValue = inputReg(elementIdx) - io.medianValue
          val shiftedValue = currentValue >> rightShiftBits
          val xorResult = shiftedValue ^ prevValue
          val leadingNum = detectLeadingZeros(xorResult)

          // Store results for this element
          parallelResults(i) := shiftedValue
          parallelLeadingNums(i) := leadingNum
          leadingNumbers(elementIdx) := leadingNum

          // Store residual bytes for this element
          when(reqBytesLength === 2.U) {
            when(leadingNum === 0.U) {
              residualBytes(residualIndex + (i * 2).U) := shiftedValue(23, 16)
              residualBytes(residualIndex + (i * 2).U + 1.U) := shiftedValue(15, 8)
            }.elsewhen(leadingNum === 1.U) {
              residualBytes(residualIndex + (i * 1).U) := shiftedValue(23, 16)
            }
          }.elsewhen(reqBytesLength === 3.U) {
            when(leadingNum === 0.U) {
              residualBytes(residualIndex + (i * 3).U) := shiftedValue(23, 16)
              residualBytes(residualIndex + (i * 3).U + 1.U) := shiftedValue(15, 8)
              residualBytes(residualIndex + (i * 3).U + 2.U) := shiftedValue(7, 0)
            }.elsewhen(leadingNum === 1.U) {
              residualBytes(residualIndex + (i * 2).U) := shiftedValue(23, 16)
              residualBytes(residualIndex + (i * 2).U + 1.U) := shiftedValue(15, 8)
            }.elsewhen(leadingNum === 2.U) {
              residualBytes(residualIndex + (i * 1).U) := shiftedValue(23, 16)
            }
          }.elsewhen(reqBytesLength === 1.U) {
            when(leadingNum === 0.U) {
              residualBytes(residualIndex + i.U) := shiftedValue(7, 0)
            }
          }.otherwise { // reqBytesLength === 4.U
            when(leadingNum === 0.U) {
              residualBytes(residualIndex + (i * 4).U) := shiftedValue(31, 24)
              residualBytes(residualIndex + (i * 4).U + 1.U) := shiftedValue(23, 16)
              residualBytes(residualIndex + (i * 4).U + 2.U) := shiftedValue(15, 8)
              residualBytes(residualIndex + (i * 4).U + 3.U) := shiftedValue(7, 0)
            }.elsewhen(leadingNum === 1.U) {
              residualBytes(residualIndex + (i * 3).U) := shiftedValue(31, 24)
              residualBytes(residualIndex + (i * 3).U + 1.U) := shiftedValue(23, 16)
              residualBytes(residualIndex + (i * 3).U + 2.U) := shiftedValue(15, 8)
            }.elsewhen(leadingNum === 2.U) {
              residualBytes(residualIndex + (i * 2).U) := shiftedValue(31, 24)
              residualBytes(residualIndex + (i * 2).U + 1.U) := shiftedValue(23, 16)
            }.otherwise { // leadingNum === 3.U
              residualBytes(residualIndex + (i * 1).U) := shiftedValue(31, 24)
            }
          }

          // Update prevValue for next iteration
          when(i.U === (parallelWidth - 1).U) {  // Changed from 3.U to 7.U for 8 elements 
            prevValue := shiftedValue
          }
        }
      }
    }

    // Simple residual index update - just increment by a fixed amount per batch
    when(state === sCompress) {
      residualIndex := residualIndex + (parallelWidth.U * reqBytesLength)  // Changed from 4.U to 8.U
    }
  }

  // State machine logic
  switch(state) {
    is(sIdle) {
      when(io.inputValid && io.inputReady) {
        printf("SZxBlockProcessor: Transitioning from sIdle to sFindStats\n")
        inputReg := io.inputData
        state := sFindStats
        busy := true.B
        processingIndex := 0.U
        parallelIndex := 0.U
        residualIndex := 0.U
        parallelResidualIndex := 0.U
        outputFormatted := false.B
        parallelProcessing := false.B
      }
    }

    is(sFindStats) {
      // Find min and max values using parallel processing
      printf("SZxBlockProcessor: sFindStats - processingIndex=%d, blockSize=%d\n", processingIndex, blockSize.U)
      when(processingIndex === 0.U) {
        printf("SZxBlockProcessor: Starting sFindStats, processingIndex=%d\n", processingIndex)
        minVal := inputReg(0)
        maxVal := inputReg(0)
        processingIndex := processingIndex + parallelWidth.U  // Process 8 elements at once **Now processes a dynamic amount of elements at once
        printf("SZxBlockProcessor: Set initial min/max, next processingIndex=%d\n", processingIndex)
      }.elsewhen(processingIndex < blockSize.U) {
        printf("SZxBlockProcessor: Parallel processing elements %d-%d\n", processingIndex, processingIndex + 7.U)
        processParallelElements(processingIndex)
        processingIndex := processingIndex + parallelWidth.U  // Process 8 elements at once
        printf("SZxBlockProcessor: Incremented processingIndex to %d\n", processingIndex)
      }.elsewhen(processingIndex >= blockSize.U) {
        // Calculate range and move to precision calculation
        printf("SZxBlockProcessor: Finished sFindStats, min=0x%x, max=0x%x\n", minVal, maxVal)
        range := maxVal - minVal
        processingIndex := 0.U
        parallelIndex := 0.U
        state := sCalculatePrecision
        printf("SZxBlockProcessor: Transitioning to sCalculatePrecision\n")
      }
    }

    is(sCalculatePrecision) {
      // Calculate precision requirements based on error bound and radius (matches C implementation)
      val radExpo = getExponentFloat(io.radius)
      val reqExpo = getPrecisionReqLength(io.errorBound)

      // C implementation: reqLength = 9 + radExpo - reqExpo + 1
      val calculatedReqLength = 9.U + radExpo - reqExpo + 1.U

      // Apply bounds like C code
      reqLength := Mux(
        calculatedReqLength < 9.U,
        9.U,
        Mux(calculatedReqLength > 32.U, 32.U, calculatedReqLength)
      )

      reqBytesLength := reqLength >> 3  // reqLength / 8
      rightShiftBits := Mux(reqLength(2, 0) === 0.U, 0.U, 8.U - reqLength(2, 0))

      state := sCompress
      processingIndex := 0.U
      parallelIndex := 0.U
      prevValue := 0.U
      printf("SZxBlockProcessor: Precision calculated, reqLength=%d, reqBytesLength=%d\n", reqLength, reqBytesLength)
    }

    is(sCompress) {
      when(processingIndex === 0.U) {
        compressionStartCycle := debugCounter
      }

      when(processingIndex < blockSize.U) {
        // Parallel SZx compression algorithm (debug output removed for performance)
        processParallelElements(processingIndex)
        processingIndex := processingIndex + parallelWidth.U  // Process 8 elements at once
      }.otherwise {
        // Compression complete, prepare output
        compressionEndCycle := debugCounter
        printf("SZxBlockProcessor: Compression took %d cycles\n", compressionEndCycle - compressionStartCycle)
        state := sOutput
        processingIndex := 0.U
        outputFormatted := false.B  // Reset output formatting flag

        // Calculate output size: header(5) + leadingNumbers + residuals
        val leadingNumbersSize = (blockSize + 3) / 4  // Ceiling division for 2-bit values
        // Always include residuals in size calculation for accuracy
        outputSizeReg := (5 + leadingNumbersSize).U + residualIndex
      }
    }

    is(sOutput) {
      // Format output: [reqLength][medianValue][leadingNumbers][residuals]
      when(!outputFormatted) {
        // Header: reqLength(1) + medianValue(4)
        outputReg(0) := reqLength(7, 0)
        outputReg(1) := io.medianValue(7, 0)
        outputReg(2) := io.medianValue(15, 8)
        outputReg(3) := io.medianValue(23, 16)
        outputReg(4) := io.medianValue(31, 24)

        // Leading numbers: pack 4x 2-bit values per byte
        val leadingNumbersSize = (blockSize + 3) / 4
        for (i <- 0 until leadingNumbersSize) {
          val packed = leadingNumbers(i * 4) << 6 |
                       leadingNumbers(i * 4 + 1) << 4 |
                       leadingNumbers(i * 4 + 2) << 2 |
                       leadingNumbers(i * 4 + 3)
          outputReg(5 + i) := packed
        }

        // Residuals: write all stored residual bytes
        val residualStartIndex = 5 + leadingNumbersSize
        val maxResidualBytes = (blockSize * 4) - residualStartIndex  // Ensure we don't exceed bounds
        for (i <- 0 until maxResidualBytes) {
          when(i.U < residualIndex) {
            outputReg(residualStartIndex + i) := residualBytes(i.U)
          }.otherwise {
            outputReg(residualStartIndex + i) := 0.U  // Zero-pad unused bytes
          }
        }
        // Zero-pad any remaining output bytes
        for (i <- residualStartIndex + maxResidualBytes until blockSize * 4) {
          outputReg(i) := 0.U
        }

        outputFormatted := true.B
        printf("SZxBlockProcessor: Output formatted, size=%d bytes\n", outputSizeReg)
      }

      // Simplified output completion - don't wait for outputReady
      when(outputFormatted) {
        printf("SZxBlockProcessor: Output complete, returning to idle\n")
        state := sIdle
        busy := false.B
        outputFormatted := false.B
      }
    }
  }
}
