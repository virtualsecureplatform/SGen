package backends

import ir.rtl.hardwaretype.{ComplexHW, FixedPoint}
import maths.fields.Complex
import org.scalatest.funsuite.AnyFunSuite

class PreservedPartitionTest extends AnyFunSuite:
  test("physical partition preservation only changes boundary attributes"):
    val hw = ComplexHW(FixedPoint(8, 12))
    val legacy = FptPartitionedTangentVerilog.emit(5, 1, 3, hw, Complex(1.0), 2)
    val preserved = FptPartitionedTangentVerilog.emit(5, 1, 3, hw, Complex(1.0), 2,
      preservePartitionRegisters = true)
    assert(preserved.replace("DONT_TOUCH = \"TRUE\", USER_SLL_REG", "USER_SLL_REG") == legacy)
    assert("DONT_TOUCH = \"TRUE\", USER_SLL_REG".r.findAllIn(preserved).size == 17)
  test("physical boundary rejects a non-paired pipeline"):
    assertThrows[IllegalArgumentException] {
      FptPartitionedTangentVerilog.emit(5, 1, 3, ComplexHW(FixedPoint(8, 12)),
        Complex(1.0), 2, boundaryRegisters = 1, preservePartitionRegisters = true)
    }
