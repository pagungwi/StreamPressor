package szx

import chisel3._
import chiseltest._
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class SZxBlockProcessorChiselTest extends AnyFlatSpec with ChiselScalatestTester with Matchers {
  "SZxBlockProcessor" should "run a simple block and print outputs" in {
    test(new SZxBlockProcessor(blockSize = 8, dataWidth = 32)) { dut =>
      // Example input: 8 values, all 42
      val inputBlock = Seq.fill(8)(42)
      // Convert to UInts
      val inputVec = inputBlock.map(_.U(32.W))
      // Set median and error bound (for demo)
      val median = 42.U(32.W)
      val errorBound = 1.U(32.W)
      val radius = 0.U(32.W)

      // Poke inputs
      dut.io.inputData.zip(inputVec).foreach { case (port, value) => port.poke(value) }
      dut.io.medianValue.poke(median)
      dut.io.errorBound.poke(errorBound)
      dut.io.radius.poke(radius)
      dut.io.inputValid.poke(true.B)
      dut.io.outputReady.poke(true.B)

      // Step until output is valid
      var cycles = 0
      while (!dut.io.outputValid.peek().litToBoolean && cycles < 20) {
        dut.clock.step(1)
        cycles += 1
      }
      dut.io.outputValid.expect(true.B)
      println(s"Output valid after $cycles cycles")

      // Print compressed output
      val outSize = dut.io.outputSize.peek().litValue
      println(s"Compressed output size: $outSize bytes")
      println("Compressed output bytes:")
      for (i <- 0 until math.min(outSize.toInt, 8)) {
        val byte = dut.io.outputData(i).peek().litValue
        print(f"$byte%02x ")
      }
      println()
    }
  }
} 