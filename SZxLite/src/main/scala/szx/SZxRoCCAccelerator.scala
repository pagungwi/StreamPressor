package szx

import chisel3._
import chisel3.util._
import freechips.rocketchip.tile._
import org.chipsalliance.cde.config._
import freechips.rocketchip.rocket._

/* Added parallelWidth and cache-aware memory access parameters */
class SZxRoCCAccelerator(opcodes: OpcodeSet, val parallelWidth: Int = 8, val cacheLineBytes: Int 64, val burstWords: Int = 16)(implicit p: Parameters)
  extends LazyRoCC(opcodes) {

  override lazy val module = new SZxRoCCAcceleratorModule(this)
}

class SZxRoCCAcceleratorModule(outer: SZxRoCCAccelerator)(implicit p: Parameters)
  extends LazyRoCCModuleImp(outer) with HasCoreParameters {
  val blockSize = 64
  require(blockSize > 0, "blockSize must be > 0")
  val moduleInstantiatedPrinted = RegInit(false.B)
  when (!moduleInstantiatedPrinted) {
    printf("RoCC: Module instantiated\n")
    moduleInstantiatedPrinted := true.B
  }

  // Configuration registers
  val errorBound = RegInit(0.U(32.W))
  val medianValue = RegInit(0.U(32.W))
  val radius = RegInit(0.U(32.W))

  // Block processor instance - this is the actual SZx compression hardware
  val blockProcessor = Module(new SZxBlockProcessor(64, 32, outer.parallelWidth, outer.cacheLineBytes))
  val wordLengthBytes = outer.cacheLineBytes / outer.burstWords
  
  // Command decoding
  val cmd_rs1 = io.cmd.bits.rs1.asUInt
  val cmd_rs2 = io.cmd.bits.rs2.asUInt
  val cmd_funct = io.cmd.bits.inst.funct
  val cmd_rd = io.cmd.bits.inst.rd

  // Store destination register when command is received
  val stored_rd = RegInit(0.U(5.W))

  // Command parameters
  val compressedSize = RegInit(0.U(32.W))

  // State machine for RoCC operations
  val sIdle :: sLoadData :: sProcessBlock :: sStoreResult :: sComplete :: sWaitResponse :: sLoadDataFromCPU :: sGetResult :: sBulkLoad :: Nil = Enum(9)
  val state = RegInit(sIdle)

  // SCRATCHPAD MEMORY - Local buffer for data processing
  val scratchpad = RegInit(VecInit(Seq.fill(blockSize)(0.U(32.W))))
  val outputScratchpad = RegInit(VecInit(Seq.fill(blockSize * 4)(0.U(8.W))))

  // Data transfer registers
  val transferIndex = RegInit(0.U(log2Ceil(blockSize).W))
  val dataIndex = RegInit(0.U(log2Ceil(blockSize).W))
  val operationTimeoutCounter = RegInit(0.U(16.W))

  // Data transfer control
  val dataTransferComplete = RegInit(false.B)

  // State transition tracking
  val justEnteredLoadState = RegInit(false.B)
  val registerUpdateDelay = RegInit(0.U(2.W))
  val addressesStored = RegInit(false.B)

  // Bulk transfer registers (new for optimization)
  val bulkTransferIndex = RegInit(0.U(8.W))
  val bulkTransferAddr = RegInit(0.U(32.W))
  // New: Cache-aware registers
  val responsesReceived = RegInit(0.U(log2Ceil(blockSize + 1).W))

  // DMA-like bulk transfer registers (disabled for now)
  // val dmaTransferActive = RegInit(false.B)
  // val dmaTransferSize = RegInit(0.U(16.W))
  // val dmaTransferAddr = RegInit(0.U(32.W))
  // val dmaTransferIndex = RegInit(0.U(8.W))
  // val dmaBuffer = RegInit(VecInit(Seq.fill(32)(0.U(32.W))))  // 32-word DMA buffer

  // Memory optimization (disabled for now)
  // val burstTransferActive = RegInit(false.B)

  // Output registers
  val outputSize = RegInit(0.U(16.W))
  val outputIndex = RegInit(0.U(log2Ceil(blockSize * 4).W))

  // Data loading from CPU control
  val dataLoadIndex = RegInit(0.U(log2Ceil(blockSize).W))
  val dataLoadValue = RegInit(0.U(32.W))

  // Debug: Track when commands fire
  when(io.cmd.valid && io.cmd.ready) {
    printf("SZxRoCC: CMD FIRE! funct=%d, rs1=0x%x, rs2=0x%x, rd=%d\n",
           io.cmd.bits.inst.funct, io.cmd.bits.rs1, io.cmd.bits.rs2, io.cmd.bits.inst.rd)
  }
  
  // Request default signal assignment
  io.mem.req.valid := false.B // default - overriden in sBulkLoad
  io.mem.req.bits.addr := 0.U
  io.mem.req.bits.cmd := M_XRD
  io.mem.req.bits.size := log2Ceil(wordLengthBytes).U
  io.mem.req.bits.tag := 0.U

  // Response handling - return the actual compressed size from hardware
  io.resp.valid := (state === sComplete)
  io.resp.bits.rd := stored_rd
  io.resp.bits.data := compressedSize

  // Busy signal - busy when not in idle state or when processing
  io.busy := (state =/= sIdle) || blockProcessor.io.busy ||
             (state === sLoadData && transferIndex < blockSize.U) ||
             (state === sLoadDataFromCPU) || (state === sGetResult) ||
             (state === sProcessBlock)

  // Interrupt (not used for now)
  io.interrupt := false.B

  // Command ready - only ready when in idle state
  io.cmd.ready := (state === sIdle)

  // Block processor connections - this is the actual SZx compression hardware
  blockProcessor.io.inputData := scratchpad
  blockProcessor.io.inputValid := (state === sProcessBlock) && blockProcessor.io.inputReady
  blockProcessor.io.outputReady := (state === sProcessBlock) || (state === sStoreResult) || (state === sComplete)  // Allow output in all states
  blockProcessor.io.errorBound := errorBound
  blockProcessor.io.medianValue := medianValue
  blockProcessor.io.radius := radius

  // State machine logic for hardware-software partitioning
  switch(state) {
    is(sIdle) {
      when(io.cmd.valid && io.cmd.ready) {
        printf("SZxRoCC: Received command, funct=%d, rs1=0x%x, rs2=0x%x, rd=%d\n",
               io.cmd.bits.inst.funct, io.cmd.bits.rs1, io.cmd.bits.rs2, io.cmd.bits.inst.rd)
        // Store destination register for response
        stored_rd := io.cmd.bits.inst.rd
        switch(io.cmd.bits.inst.funct) {
          is(0.U) { // CONFIG: rs1=errorBound, rs2=medianValue
            printf("SZxRoCC: Processing CONFIG command\n")
            errorBound := io.cmd.bits.rs1
            medianValue := io.cmd.bits.rs2
            compressedSize := 0.U
            state := sComplete
            printf("SZxRoCC: CONFIG command completed\n")
          }
          is(1.U) { // SET_RADIUS: rs1=radius
            printf("SZxRoCC: Processing SET_RADIUS command\n")
            radius := io.cmd.bits.rs1
            compressedSize := 0.U
            state := sComplete
            printf("SZxRoCC: SET_RADIUS command completed\n")
          }
                  is(2.U) { // COMPRESS_BLOCK: rs1=input_addr, rs2=output_addr
          printf("SZxRoCC: Processing COMPRESS_BLOCK command - Hardware acceleration\n")
          printf("SZxRoCC: Input addr=0x%x, Output addr=0x%x\n", io.cmd.bits.rs1, io.cmd.bits.rs2)

          // Data should already be in scratchpad from LOAD_DATA commands
          // Go directly to processing since data is already loaded
          printf("SZxRoCC: Data already in scratchpad, starting hardware compression directly\n")
          dataIndex := 0.U
          state := sProcessBlock
        }
          is(3.U) { // COMPRESS_MULTI: rs1=input_addr, rs2=output_addr
            printf("SZxRoCC: Processing COMPRESS_MULTI command - Hardware acceleration\n")
            // For multi-block, we'll process one block at a time
            transferIndex := 0.U
            dataTransferComplete := false.B
            state := sLoadData
          }
          is(4.U) { // LOAD_DATA: rs1=data_value, rs2=index
            printf("SZxRoCC: Processing LOAD_DATA command - Loading data to scratchpad\n")
            printf("SZxRoCC: Data value=0x%x, Index=%d\n", io.cmd.bits.rs1, io.cmd.bits.rs2)
            dataLoadValue := io.cmd.bits.rs1
            dataLoadIndex := io.cmd.bits.rs2
            state := sLoadDataFromCPU
          }
          is(5.U) {  // SZX_GET_RESULT
            printf("SZxRoCC: Processing GET_RESULT command - Returning compressed result\n")
            compressedSize := outputSize
            state := sComplete
          }
          is(6.U) {  // SZX_LOAD_BLOCK - Bulk block loading
            printf("SZxRoCC: Processing LOAD_BLOCK command - Bulk loading from memory\n")
            val blockStartAddr = io.cmd.bits.rs1
            val blockSize = io.cmd.bits.rs2
            printf("SZxRoCC: Loading block from addr=0x%x, size=%d\n", blockStartAddr, blockSize)

            // Use DMA-like bulk transfer to load entire block at once
            // This eliminates 64 individual RoCC calls per block
            bulkTransferAddr := blockStartAddr
            bulkTransferIndex := 0.U
            responsesReceived := 0.U // reset responses received for BulkLoad
            state := sBulkLoad
          }
        }
      }
    }
    is(sLoadData) {
      printf("SZxRoCC: Loading data to scratchpad for hardware processing\n")
      printf("SZxRoCC: Current transferIndex=%d\n", transferIndex)

      // Increment timeout counter
      operationTimeoutCounter := operationTimeoutCounter + 1.U

      // Clear the flag after first cycle to allow data transfer
      when(justEnteredLoadState) {
        justEnteredLoadState := false.B
        printf("SZxRoCC: First cycle in load state, starting data transfer\n")
      }

      // Add a delay to ensure register updates take effect
      when(justEnteredLoadState) {
        registerUpdateDelay := 0.U
      }.otherwise {
        registerUpdateDelay := registerUpdateDelay + 1.U
      }

      // Transfer data from CPU memory to scratchpad via registers
      // The CPU has already loaded real data via LOAD_DATA commands
      // Now we just need to wait for the data to be available and start processing
      when(transferIndex < blockSize.U && registerUpdateDelay >= 2.U) {
        // Data should already be in scratchpad from LOAD_DATA commands
        printf("SZxRoCC: Using real data from scratchpad[%d]=0x%x\n", transferIndex, scratchpad(transferIndex))
        transferIndex := transferIndex + 1.U
      }

      // When all data is loaded, start hardware compression
      when(transferIndex >= blockSize.U) {
        printf("SZxRoCC: All data loaded to scratchpad, starting hardware SZx compression\n")
        dataIndex := 0.U
        state := sProcessBlock
      }

      // Timeout protection - if data transfer takes too long, return dummy result
      when(operationTimeoutCounter > 1000.U) {
        printf("SZxRoCC: Data transfer timeout after %d cycles, returning dummy result\n", operationTimeoutCounter)
        compressedSize := 64.U // Dummy compressed size
        state := sComplete
      }
    }
    is(sProcessBlock) {
      when(dataIndex === 0.U) {
        printf("SZxRoCC: Starting hardware compression, dataIndex=%d\n", dataIndex)
      }

      // Wait for hardware compression to complete
      when(blockProcessor.io.outputValid) {
        printf("SZxRoCC: Hardware compression completed!\n");
        printf("SZxRoCC: Output size from hardware: %d bytes\n", blockProcessor.io.outputSize);
        outputScratchpad := blockProcessor.io.outputData
        outputSize := blockProcessor.io.outputSize
        compressedSize := blockProcessor.io.outputSize
        state := sStoreResult
      }

      // Timeout protection - only if hardware doesn't respond
      when(dataIndex > 10000.U) {  // Increased timeout to allow real compression
        printf("SZxRoCC: Hardware timeout after %d cycles, using fallback\n", dataIndex);
        compressedSize := 9999.U // Fallback compressed size
        state := sStoreResult
      }

      dataIndex := dataIndex + 1.U
    }
    is(sStoreResult) {
      printf("SZxRoCC: Compressed data ready in scratchpad\n")
      printf("SZxRoCC: Output size: %d bytes\n", outputSize)

      // For now, we'll just return the compressed size
      // In a real implementation, the CPU would read the compressed data via RoCC commands
      printf("SZxRoCC: Compressed data available in scratchpad for CPU to read\n")
      state := sComplete
    }
    is(sComplete) {
      printf("SZxRoCC: Hardware acceleration completed, returning compressed size: %d\n", compressedSize)
      // Move to wait response state to ensure proper timing
      state := sWaitResponse
    }
    is(sWaitResponse) {
      // Send response when ready
      when(io.resp.ready) {
        printf("SZxRoCC: Response sent to CPU, compressed size: %d bytes\n", compressedSize)
        state := sIdle
      }
    }
    is(sLoadDataFromCPU) {
      printf("SZxRoCC: Loading data from CPU to scratchpad[%d] = 0x%x\n", dataLoadIndex, dataLoadValue)
      // Store the data value in the scratchpad at the specified index
      scratchpad(dataLoadIndex) := dataLoadValue
      compressedSize := 0.U // No compression result for data loading
      state := sComplete
    }
    is(sGetResult) {
      printf("SZxRoCC: Returning compressed result: %d bytes\n", outputSize)
      // Return the stored compressed size
      compressedSize := outputSize
      state := sComplete
    }
    /* In Bulk Load state: request entire block of data and store in local high-speed buffer/scratchpad */
    is(sBulkLoad) {
      
      // Load 4 words per cycle (16 bytes per cycle)
      when(bulkTransferIndex < blockSize.U) {
        // Replace simulated block transfer with actual load using RoCC mem port
            // io.mem.req: wires between accelerator (requester) and memory system (responder) - valid/ready handshake
        io.mem.req.valid := true.B // accelerator ready to receive data from memory system
        io.mem.req.bits.tag := bulkTransferIndex // use word index as tag
        io.mem.req.bits.addr := bulkTransferAddr + (bulkTransferIndex << 2)
        io.mem.req.bits.cmd := M_XRD // memory read
        io.mem.req.bits.size := log2Ceil(wordLengthBytes).U // calculate word granularity (4B default)
        
        when(io.mem.req.fire) { // check valid and ready
          bulkTransferIndex := bulkTransferIndex + 1.U
        }
      }
      
      // Capture responses, use tag matching for handling out-of-order reception
      when(io.mem.resp.valid) {
        scratchpad(io.mem.resp.bits.tag) := io.mem.resp.bits.data(wordLengthBytes*8-1, 0)
        responsesReceived := responsesReceived + 1.U // increment counter on valid receive
      }
      
      when(bulkTransferIndex >= blockSize.U && responsesReceived >= blockSize.U) {
        // Bulk transfer complete, start compression
        printf("SZxRoCC: Bulk transfer complete, starting compression\n")
        state := sProcessBlock // advance to next state
        dataIndex := 0.U
      }
    }
  }

  // Debug output for hardware-software partitioning
  when(state === sProcessBlock) {
    printf("SZxRoCC: Hardware compression state - outputSize=%d, outputValid=%d, busy=%d, inputReady=%d\n",
           blockProcessor.io.outputSize, blockProcessor.io.outputValid, blockProcessor.io.busy, blockProcessor.io.inputReady)
  }

  when(state === sStoreResult) {
    printf("SZxRoCC: Storing hardware compression result, outputSize=%d\n", outputSize)
  }
}
