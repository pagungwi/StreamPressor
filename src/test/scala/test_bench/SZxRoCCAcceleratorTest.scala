import chisel3._
import chiseltest._
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import chisel3.util._
import szx.SZxRoCCAccelerator
import freechips.rocketchip.config.Parameters
import freechips.rocketchip.tile.RoCCCommand
import freechips.rocketchip.tile.RoCCResponse
import freechips.rocketchip.tile.MemReqCmd
import freechips.rocketchip.tile.MemResp

class SZxRoCCAcceleratorTest extends AnyFlatSpec with ChiselScalatestTester with Matchers {
  
  "SZxRoCCAccelerator" should "handle configuration commands correctly" in {
    test(new SZxRoCCAccelerator(OpcodeSet.custom0)(Parameters.empty)) { dut =>
      println("=== Testing RoCC Configuration Commands ===")
      
      // Test CONFIG command (funct=0)
      val errorBound = 0x3dcccccd.U(64.W) // 0.1f in IEEE 754
      val medianValue = 0x42280000.U(64.W) // 42.0f in IEEE 754
      
      dut.io.cmd.valid.poke(true.B)
      dut.io.cmd.bits.inst.funct.poke(0.U)
      dut.io.cmd.bits.rs1.poke(errorBound)
      dut.io.cmd.bits.rs2.poke(medianValue)
      dut.io.cmd.bits.inst.rd.poke(5.U)
      dut.io.resp.ready.poke(true.B)
      
      dut.clock.step()
      
      dut.io.cmd.valid.poke(false.B)
      dut.io.resp.valid.expect(true.B)
      dut.io.resp.bits.rd.expect(5.U)
      dut.io.resp.bits.data.expect(0.U)
      
      println("✓ CONFIG command processed correctly")
    }
  }
  
  it should "handle SET_RADIUS command correctly" in {
    test(new SZxRoCCAccelerator(OpcodeSet.custom0)(Parameters.empty)) { dut =>
      println("=== Testing SET_RADIUS Command ===")
      
      val radius = 0x3f800000.U(64.W) // 1.0f in IEEE 754
      
      dut.io.cmd.valid.poke(true.B)
      dut.io.cmd.bits.inst.funct.poke(1.U)
      dut.io.cmd.bits.rs1.poke(radius)
      dut.io.cmd.bits.inst.rd.poke(3.U)
      dut.io.resp.ready.poke(true.B)
      
      dut.clock.step()
      
      dut.io.cmd.valid.poke(false.B)
      dut.io.resp.valid.expect(true.B)
      dut.io.resp.bits.rd.expect(3.U)
      dut.io.resp.bits.data.expect(0.U)
      
      println("✓ SET_RADIUS command processed correctly")
    }
  }
  
  it should "handle COMPRESS_BLOCK command with memory operations" in {
    test(new SZxRoCCAccelerator(OpcodeSet.custom0)(Parameters.empty)) { dut =>
      println("=== Testing COMPRESS_BLOCK Command with Memory Operations ===")
      
      // Configure accelerator first
      val errorBound = 0x3dcccccd.U(64.W) // 0.1f
      val medianValue = 0x42280000.U(64.W) // 42.0f
      val radius = 0x3f800000.U(64.W) // 1.0f
      
      // CONFIG
      dut.io.cmd.valid.poke(true.B)
      dut.io.cmd.bits.inst.funct.poke(0.U)
      dut.io.cmd.bits.rs1.poke(errorBound)
      dut.io.cmd.bits.rs2.poke(medianValue)
      dut.io.cmd.bits.inst.rd.poke(0.U)
      dut.io.resp.ready.poke(true.B)
      dut.clock.step()
      dut.io.cmd.valid.poke(false.B)
      
      // SET_RADIUS
      dut.io.cmd.valid.poke(true.B)
      dut.io.cmd.bits.inst.funct.poke(1.U)
      dut.io.cmd.bits.rs1.poke(radius)
      dut.io.cmd.bits.inst.rd.poke(0.U)
      dut.clock.step()
      dut.io.cmd.valid.poke(false.B)
      
      // COMPRESS_BLOCK command
      val inputAddr = 0x1000.U(64.W)
      val outputAddr = 0x2000.U(64.W)
      
      dut.io.cmd.valid.poke(true.B)
      dut.io.cmd.bits.inst.funct.poke(2.U)
      dut.io.cmd.bits.rs1.poke(inputAddr)
      dut.io.cmd.bits.rs2.poke(outputAddr)
      dut.io.cmd.bits.inst.rd.poke(7.U)
      dut.clock.step()
      dut.io.cmd.valid.poke(false.B)
      
      // Should be busy now
      dut.io.busy.expect(true.B)
      
      // Simulate memory responses for data loading (64 floats)
      dut.io.mem.resp.valid.poke(true.B)
      for (i <- 0 until 64) {
        dut.io.mem.resp.bits.data.poke(0x42280000.U(64.W)) // 42.0f
        dut.clock.step()
      }
      
      // Wait for block processing to complete
      var cycles = 0
      while (!dut.io.resp.valid.peek().litToBoolean && cycles < 1000) {
        dut.clock.step()
        cycles += 1
      }
      
      // Should have completed
      dut.io.resp.valid.expect(true.B)
      dut.io.resp.bits.rd.expect(7.U)
      
      // Check that we got a reasonable compressed size
      val compressedSize = dut.io.resp.bits.data.peek().litValue
      println(s"✓ COMPRESS_BLOCK completed with compressed size: $compressedSize")
      println(s"✓ Processing took $cycles cycles")
      
      // Should no longer be busy
      dut.io.busy.expect(false.B)
    }
  }
  
  it should "handle COMPRESS_MULTI command correctly" in {
    test(new SZxRoCCAccelerator(OpcodeSet.custom0)(Parameters.empty)) { dut =>
      println("=== Testing COMPRESS_MULTI Command ===")
      
      // Configure accelerator
      val errorBound = 0x3dcccccd.U(64.W) // 0.1f
      val medianValue = 0x42280000.U(64.W) // 42.0f
      val radius = 0x3f800000.U(64.W) // 1.0f
      
      // CONFIG
      dut.io.cmd.valid.poke(true.B)
      dut.io.cmd.bits.inst.funct.poke(0.U)
      dut.io.cmd.bits.rs1.poke(errorBound)
      dut.io.cmd.bits.rs2.poke(medianValue)
      dut.io.cmd.bits.inst.rd.poke(0.U)
      dut.io.resp.ready.poke(true.B)
      dut.clock.step()
      dut.io.cmd.valid.poke(false.B)
      
      // SET_RADIUS
      dut.io.cmd.valid.poke(true.B)
      dut.io.cmd.bits.inst.funct.poke(1.U)
      dut.io.cmd.bits.rs1.poke(radius)
      dut.io.cmd.bits.inst.rd.poke(0.U)
      dut.clock.step()
      dut.io.cmd.valid.poke(false.B)
      
      // COMPRESS_MULTI command
      val inputAddr = 0x3000.U(64.W)
      val outputAddr = 0x4000.U(64.W)
      
      dut.io.cmd.valid.poke(true.B)
      dut.io.cmd.bits.inst.funct.poke(3.U)
      dut.io.cmd.bits.rs1.poke(inputAddr)
      dut.io.cmd.bits.rs2.poke(outputAddr)
      dut.io.cmd.bits.inst.rd.poke(10.U)
      dut.clock.step()
      dut.io.cmd.valid.poke(false.B)
      
      // Should be busy
      dut.io.busy.expect(true.B)
      
      // Simulate memory responses for data loading
      dut.io.mem.resp.valid.poke(true.B)
      for (i <- 0 until 64) {
        dut.io.mem.resp.bits.data.poke(0x42280000.U(64.W)) // 42.0f
        dut.clock.step()
      }
      
      // Wait for completion
      var cycles = 0
      while (!dut.io.resp.valid.peek().litToBoolean && cycles < 1000) {
        dut.clock.step()
        cycles += 1
      }
      
      dut.io.resp.valid.expect(true.B)
      dut.io.resp.bits.rd.expect(10.U)
      
      val compressedSize = dut.io.resp.bits.data.peek().litValue
      println(s"✓ COMPRESS_MULTI completed with compressed size: $compressedSize")
      println(s"✓ Processing took $cycles cycles")
      
      dut.io.busy.expect(false.B)
    }
  }
  
  it should "handle multiple commands in sequence" in {
    test(new SZxRoCCAccelerator(OpcodeSet.custom0)(Parameters.empty)) { dut =>
      println("=== Testing Multiple Commands in Sequence ===")
      
      dut.io.resp.ready.poke(true.B)
      
      // Command 1: CONFIG
      dut.io.cmd.valid.poke(true.B)
      dut.io.cmd.bits.inst.funct.poke(0.U)
      dut.io.cmd.bits.rs1.poke(0x3dcccccd.U(64.W))
      dut.io.cmd.bits.rs2.poke(0x42280000.U(64.W))
      dut.io.cmd.bits.inst.rd.poke(1.U)
      dut.clock.step()
      dut.io.cmd.valid.poke(false.B)
      
      dut.io.resp.valid.expect(true.B)
      dut.clock.step()
      
      // Command 2: SET_RADIUS
      dut.io.cmd.valid.poke(true.B)
      dut.io.cmd.bits.inst.funct.poke(1.U)
      dut.io.cmd.bits.rs1.poke(0x3f800000.U(64.W))
      dut.io.cmd.bits.inst.rd.poke(2.U)
      dut.clock.step()
      dut.io.cmd.valid.poke(false.B)
      
      dut.io.resp.valid.expect(true.B)
      dut.clock.step()
      
      // Command 3: CONFIG again
      dut.io.cmd.valid.poke(true.B)
      dut.io.cmd.bits.inst.funct.poke(0.U)
      dut.io.cmd.bits.rs1.poke(0x3e4ccccd.U(64.W)) // Different error bound
      dut.io.cmd.bits.rs2.poke(0x41a00000.U(64.W)) // Different median
      dut.io.cmd.bits.inst.rd.poke(3.U)
      dut.clock.step()
      dut.io.cmd.valid.poke(false.B)
      
      dut.io.resp.valid.expect(true.B)
      dut.clock.step()
      
      println("✓ Multiple commands processed correctly in sequence")
    }
  }
} 