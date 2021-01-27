package examples.leros

import chisel3.tester.experimental.TestOptionBuilder.ChiselScalatestOptionBuilder
import chisel3.tester.{testableClock, testableData}
import chiseltest.ChiselScalatestTester
import chiseltest.internal.VerilatorBackendAnnotation
import chiselverify.coverage.{Bins, CoverPoint, CoverageReporter}
import examples.leros.Types.ld
import org.scalatest.{FlatSpec, Matchers}
import chisel3._
import chisel3.util._



class AluAccuTesterMulti extends FlatSpec with ChiselScalatestTester with Matchers {

  def testFun[T <: AluAccu](dut: T): Unit = {

    val cr = new CoverageReporter(dut)
    cr.register(List(
      CoverPoint(dut.io.din , "din") {
        List(Bins("lo10", 0 to 10),Bins("First100", 0 to 100000))
      },
      CoverPoint(dut.io.accu, "accu") {
        List(Bins("lo10", 0 to 10), Bins("First100", 0 to 100000))
      },
      CoverPoint(dut.io.op, "op") {
        List(Bins("nop", 0 to 0), Bins("add", 1 to 1),
          Bins("sub", 2 to 2), Bins("and", 3 to 3),
          Bins("or", 4 to 4), Bins("xor", 5 to 5),
          Bins("shr", 6 to 6), Bins("ld", 7 to 7))
      }))
    //Declare cross points
    /*Cross("accuAndTest", "accu", "test",
        CrossBin("both1", 1 to 1, 1 to 1)::Nil)::
    Nil)*/

    def alu(a: Int, b: Int, op: Int): Int = {

      op match {
        case 0 => a
        case 1 => a + b
        case 2 => a - b
        case 3 => a & b
        case 4 => a | b
        case 5 => a ^ b
        case 6 => b
        case 7 => a >>> 1
        case _ => -123 // This shall not happen
      }
    }

    def toUInt(i: Int) = (BigInt(i) & 0x00ffffffffL).asUInt(32.W)

    def testOne(a: Int, b: Int, fun: Int): Unit = {
      dut.io.op.poke(ld)
      dut.io.ena.poke(true.B)
      dut.io.din.poke(toUInt(a))
      dut.clock.step()
      dut.io.op.poke(fun.asUInt())
      dut.io.din.poke(toUInt(b))
      dut.clock.step()
      dut.io.accu.expect(toUInt(alu(a, b, fun)))

      cr.sample()
    }

    def test(values: Seq[Int]): Unit = {
      // for (fun <- add to shr) {
      for (fun <- 0 until 8) {
        for (a <- values) {
          for (b <- values) {
            testOne(a, b, fun)
          }
        }
      }
    }

    // Some interesting corner cases
    val interesting = Array(1, 2, 4, -123, 123, 0, -1, -2, 0x80000000, 0x7fffffff)
    test(interesting)

    val randArgs = Seq.fill(20)(scala.util.Random.nextInt)
    test(randArgs)

    //Print coverage report
    cr.printReport()
  }


  "AluAccuChisel" should "pass" in {
    test(new AluAccuChisel(32)){ dut => testFun(dut) }
  }

  "AluAccuGenerated" should "pass" in {
    test(new AluAccuGenerated(32)).withAnnotations(Seq(VerilatorBackendAnnotation)) { dut => testFun(dut) }
  }
}
