package chiselverify.crv.backends.jacop.experimental

import chisel3.{Bool, SInt, UInt}
import chiselverify.crv.backends.jacop.{Constraint, Model, Rand}
import org.jacop.constraints.XplusCeqZ
import org.jacop.core.IntDomain

class RandChisel(name: String, min: Int, max: Int)(override implicit val model: Model) extends Rand(name, min, max) {

  /** Defines add [[Constraint]] between Rand and an integer value.
    *
    * @param that a UInt parameter for the addition [[Constraint]].
    * @return [[Rand]] variable being the result of the addition [[Constraint]].
    */
  def #+(that: UInt): Rand = {
    require(that.getWidth <= 30)
    require(that.isLit())
    val result =
      new Rand(IntDomain.addInt(this.min(), that.litValue().toInt), IntDomain.addInt(this.max(), that.litValue().toInt))
    val c = new XplusCeqZ(this, that.litValue().toInt, result)
    model.crvconstr += new Constraint(c)
    result
  }

  /** Defines add [[Constraint]] between Rand and an integer value.
    *
    * @param that a SInt parameter for the addition [[Constraint]].
    * @return [[Rand]] variable being the result of the addition [[Constraint]].
    */
  def #+(that: SInt): Rand = {
    require(that.getWidth <= 30)
    require(that.isLit())
    val result =
      new Rand(IntDomain.addInt(this.min(), that.litValue().toInt), IntDomain.addInt(this.max(), that.litValue().toInt))
    val c = new XplusCeqZ(this, that.litValue().toInt, result)
    model.crvconstr += new Constraint(c)
    result
  }

  /** Defines add [[Constraint]] between Rand and an integer value.
    *
    * @param that a Bool parameter for the addition [[Constraint]].
    * @return [[Rand]] variable being the result of the addition [[Constraint]].
    */
  def #+(that: Bool): Rand = {
    val result =
      new Rand(IntDomain.addInt(this.min(), that.litValue().toInt), IntDomain.addInt(this.max(), that.litValue().toInt))
    val c = new XplusCeqZ(this, that.litValue().toInt, result)
    model.crvconstr += new Constraint(c)
    result
  }

  /** Defines subtract [[Constraint]] between [[Rand]] and an integer value.
    *
    * @param that a [[chisel3.UInt]] parameter for the subtraction [[Constraint]].
    * @return [[Rand]] variable being the result of the subtraction [[Constraint]].
    */
  def #-(that: UInt): Rand = {
    require(that.getWidth <= 30)
    val result = new Rand(
      IntDomain.subtractInt(this.min(), that.litValue().toInt),
      IntDomain.subtractInt(this.max(), that.litValue().toInt)
    )
    val c = new XplusCeqZ(result, that.litValue().toInt, this)
    val crvc = new Constraint(c)
    model.crvconstr += crvc
    result
  }

  /** Defines subtract [[Constraint]] between [[Rand]] and an integer value.
    *
    * @param that a [[chisel3.SInt]] parameter for the subtraction [[Constraint]].
    * @return [[Rand]] variable being the result of the subtraction [[Constraint]].
    */
  def #-(that: SInt): Rand = {
    require(that.getWidth <= 30)
    val result = new Rand(
      IntDomain.subtractInt(this.min(), that.litValue().toInt),
      IntDomain.subtractInt(this.max(), that.litValue().toInt)
    )
    val c = new XplusCeqZ(result, that.litValue().toInt, this)
    val crvc = new Constraint(c)
    model.crvconstr += crvc
    result
  }

  /** Defines subtract [[Constraint]] between [[Rand]] and an integer value.
    *
    * @param that a [[chisel3.Bool]] parameter for the subtraction [[Constraint]].
    * @return [[Rand]] variable being the result of the subtraction [[Constraint]].
    */
  def #-(that: Bool): Rand = {
    require(that.isLit())
    val result = new Rand(
      IntDomain.subtractInt(this.min(), that.litValue().toInt),
      IntDomain.subtractInt(this.max(), that.litValue().toInt)
    )
    val c = new XplusCeqZ(result, that.litValue().toInt, this)
    val crvc = new Constraint(c)
    model.crvconstr += crvc
    result
  }
}
