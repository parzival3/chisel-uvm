package verifyTests.crv.backends.jacop.bundle.random

import chisel3.util._
import chisel3._

import chiseltest.ChiselScalatestTester
import examples.leros.Types.{add, and, ld, nop, or, shr, sub, xor}
import org.scalatest.FlatSpec

class Test extends FlatSpec with ChiselScalatestTester {

  class AluAccuInputPort(val size: Int) extends Bundle{
    val op = UInt(3.W)
    val din = UInt(size.W)
    val ena = Bool()
  }

  class AluAccuOutPort(val size: Int) extends Bundle {
    val accu = UInt(size.W)
  }

  /**
    * Base class for Leros ALU including the accumulator register.
    *
    * @param size
    */
  abstract class AluAccuMutliIO(sizen: Int) extends MultiIOModule {
    val input = IO(Input(new AluAccuInputPort(sizen)))
    val output = IO(Output(new AluAccuOutPort(sizen)))
  }

  class AluAccuMultiChisel(val size: Int) extends AluAccuMutliIO(size) {

    val accuReg = RegInit(0.U(size.W))

    val op = input.op
    val a = accuReg
    val b = input.din
    val res = WireDefault(a)

    switch(op) {
      is(nop) {
        res := a
      }
      is(add) {
        res := a + b
      }
      is(sub) {
        res := a - b
      }
      is(and) {
        res := a & b
      }
      is(or) {
        res := a | b
      }
      is(xor) {
        res := a ^ b
      }
      is (shr) {
        res := a >> 1
      }
      is(ld) {
        res := b
      }
    }

    when (input.ena) {
      accuReg := res
    }

    output.accu := accuReg
  }

}
