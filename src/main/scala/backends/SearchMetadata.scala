package backends

import ir.rtl.StreamingModule
import ir.rtl.hardwaretype.{ComplexHW, FixedPoint}
import transforms.fft.DFT
import java.nio.file.{Files, Paths}

/** Declared contracts, independently checked by the external RTL evaluator. */
object SearchMetadata:
  def write[T](sm: StreamingModule[T], family: String, radixLog: Int, scale: String,
      control: String, top: String, file: String): Unit =
    require(FixedPoint.strictRounding.value, "metadata requires -strict-fixedpoint")
    val data = sm.hw match
      case ComplexHW(f: FixedPoint) => f
      case _ => throw IllegalArgumentException("search metadata requires complex fixed-point arithmetic")
    require(Set("CTDFT", "ICTDFT", "ItPeaseFused", "IItPeaseFused")(family), "unsupported FFT family")
    require(radixLog == 1 && scale == "1" && control == "Dual",
      "certifiable metadata currently requires radix 2, unit scaling, dual RAM control")
    val tw = FixedPoint(2, data.size - 2)
    val entries = (0 until sm.N).map { i =>
      val z = DFT.omega(sm.n, i)
      s"""{"exponent":$i,"real_bits":"${tw.bitsOf(z.re)}","imag_bits":"${tw.bitsOf(z.im)}"}"""
    }.mkString(",")
    val hash = java.security.MessageDigest.getInstance("SHA-256").digest(Files.readAllBytes(Paths.get(file)))
      .map(b => f"${b & 0xff}%02x").mkString
    val json = s"""{
      |"schema":"sgen-search-v1","numeric_model":"sgen-radix2-v1",
      |"family":"$family","top":"$top","transform_size":${sm.N},"streaming_width":${sm.K},
      |"latency":${sm.latency},"next_offset":${sm.nextAt},"input_cycles":${sm.T},"output_cycles":${sm.T},
      |"initiation_interval":${sm.T + sm.minGap},"minimum_gap":${sm.minGap},"ram_control":"$control",
      |"radix":2,"scaling":"1","integer_bits":${data.magnitude},"fractional_bits":${data.fractional},
      |"twiddle_integer_bits":2,"twiddle_fractional_bits":${tw.fractional},"complex_packing":"imag-high-real-low",
      |"multiply_rounding":"signed-floor-after-each-real-product","addition":"fixed-width-wrap",
      |"twiddles":[$entries],"dependencies":[],"rtl_sha256":"$hash",
      |"operation_contract":${OperationContract.json(sm)}
      |}""".stripMargin
    Files.write(Paths.get(file.replaceFirst("\\.[^.]+$", "") + ".json"), json.getBytes("UTF-8"))
