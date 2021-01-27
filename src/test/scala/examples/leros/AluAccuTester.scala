package examples.leros

import chisel3._
import chisel3.experimental.BundleLiterals.AddBundleLiteralConstructor
import chiseltest._
import chiselverify.coverage.{Bins, CoverPoint, CoverageReporter}
import org.scalatest._
import chiselverify.crv.backends.jacop.{Model, Rand, RandObj}
import examples.leros.Types._

import scala.math.pow

class AluTransaction(seed: Int, size: Int) extends RandObj {
  currentModel = new Model(seed)
  val max = pow(2, size).toInt
  val op = new Rand("op", 0, 7)
  val din = new Rand("din", 0, max)
  val ena = new Rand("ena", 0, 1)

  def toBundle: AluAccuInputPort = {
    new AluAccuInputPort(size).Lit(_.op -> op.value().U, _.din -> din.value().U, _.ena -> ena.value().B)
  }
}

trait AluBehavior {
  this: FlatSpec with ChiselScalatestTester =>

  def testAlu(seed: Int, n: Int, s: Int, transactions: List[(AluAccuInputPort, AluAccuInputPort)]): Unit = {
    val mask: Int = (1 << s) - 1

    def alu(a: Int, b: Int, op: Int): AluAccuOutPort = {
      val result = op match {
        case 0 => a.U
        case 1 => if ((a + b) > mask) ((a + b) & mask).U else (a + b).U
        case 2 => if ((a - b) < 0) ((a - b) & mask).U else (a - b).U
        case 3 => (a & b).U
        case 4 => (a | b).U
        case 5 => (a ^ b).U
        case 6 => b.U
        case 7 => (a >>> 1).U
      }
      new AluAccuOutPort(s).Lit(_.accu -> result)
    }

    it should s"test alu with seed = $seed and number = $n" in {
      test(new AluAccuMultiChisel(s)) { dut =>

        val cr = new CoverageReporter(dut)
        cr.register(List(
          CoverPoint(dut.input.din, "din") {
            List(Bins("lo10", 0 to 10), Bins("First100", 0 to 100000))
          },
          CoverPoint(dut.output.accu, "accu") {
            List(Bins("lo10", 0 to 10), Bins("First100", 0 to 100000))
          },
          CoverPoint(dut.input.op, "op") {
            List(Bins("nop", 0 to 0), Bins("add", 1 to 1),
              Bins("sub", 2 to 2), Bins("and", 3 to 3),
              Bins("or", 4 to 4), Bins("xor", 5 to 5),
              Bins("shr", 6 to 6), Bins("ld", 7 to 7))
          }))

        transactions.foreach { case (t1, t2) =>
          dut.input.poke(t1)
          dut.clock.step()
          dut.input.poke(t2)
          dut.clock.step()
          dut.output.expect(alu(t1.din.litValue().toInt, t2.din.litValue().toInt, t2.op.litValue().toInt))
          println(dut.input.peek().op.litValue())
          cr.sample()
        }
        cr.printReport()

      }
    }
  }
}

class AluAccuTester extends FlatSpec with ChiselScalatestTester with Matchers {
  val seeds = List( 30, 40, 800, 400, 600)

  def testFun[T <: AluAccuMutliIO](dut: T, seeds: List[Int], nTransactions: Int): Unit = {

    val cr = new CoverageReporter(dut)
    val operations = new CoverageReporter(dut)

    cr.register(List(
      CoverPoint(dut.input.din, "din") {
        List(Bins("lo10", 0 to 10), Bins("First100", 0 to 100000))
      },
      CoverPoint(dut.output.accu, "accu") {
        List(Bins("lo10", 0 to 10), Bins("First100", 0 to 100000))
      }))

    operations.register(List(
      CoverPoint(dut.input.op, "op") {
        List(Bins("nop", 0 to 0), Bins("add", 1 to 1),
          Bins("sub", 2 to 2), Bins("and", 3 to 3),
          Bins("or", 4 to 4), Bins("xor", 5 to 5),
          Bins("shr", 6 to 6), Bins("ld", 7 to 7))
      }))

    def alu(t1: AluAccuInputPort, t2: AluAccuInputPort): AluAccuOutPort = {
      val a = t1.din.litValue().toInt
      val b = t2.din.litValue().toInt
      val op = t2.op.litValue().toInt
      val mask: Int = (1 << t1.din.getWidth) - 1

      val result = op match {
        case 0 => a.U
        case 1 => if ((a + b) > mask) ((a + b) & mask).U else (a + b).U
        case 2 => if ((a - b) < 0) ((a - b) & mask).U else (a - b).U
        case 3 => (a & b).U
        case 4 => (a | b).U
        case 5 => (a ^ b).U
        case 6 => b.U
        case 7 => (a >>> 1).U
      }
      new AluAccuOutPort(t1.din.getWidth).Lit(_.accu -> result)
    }

    def testOne(t1: AluAccuInputPort, t2: AluAccuInputPort): Unit = {
      dut.input.poke(t1)
      dut.clock.step()
      dut.input.poke(t2)
      dut.clock.step()
      dut.output.expect(alu(t1, t2))
      cr.sample()
      operations.sample()
    }

    def transactions(seed: Int, n: Int, s: Int) = {
      val t1 = new AluTransaction(seed, s)
      val t2 = new AluTransaction(seed, s)
      Range(0, n).map { _ =>
        t1.randomize_with(t1.op #= ld.litValue(), t1.ena #= 1)
        t2.randomize_with(t2.ena #= 1)
        (t1.toBundle, t2.toBundle)
      }.toList
    }

    def test(seeds: Seq[Int]): Unit = {
      seeds foreach { s =>
        transactions(s, nTransactions, dut.input.din.getWidth) foreach { case (t1, t2) =>
          testOne(t1, t2)
        }
      }
    }

    // Some interesting corner cases
    test(seeds)
    cr.printReport()
    operations.printReport()
  }

  "AluAccuChisel" should "pass" in {
    test(new AluAccuMultiChisel(16)){ dut => testFun(dut, seeds, 100) }
  }
}
