package backends

import ir.rtl.*

/** Snapshot of the final lowered graph, including state and bit selection.
  * Consumers must independently check it against RTL and numerical semantics.
  */
object OperationContract:
  def json(mod: Module): String =
    val nodes = mod.components.toVector
    val ids = nodes.zipWithIndex.toMap
    val indexes = nodes.zip(nodes.map {
      case _: Input | _: Output | _: Wire | _: Const => 0
      case Register(_, cycles) if cycles > 1 => 2
      case RAM(_, _, _) => 2
      case _ => 1
    }.scanLeft(1)(_ + _)).toMap
    def quote(s: String): String =
      require(!s.exists(c => c == '"' || c == '\\' || c < ' '))
      "\"" + s + "\""
    def name(c: Component): String = c match
      case Input(_, n) => n
      case Output(_, n) => n
      case Wire(input) => name(input)
      case Const(size, value) => s"${size}'d$value"
      case _ => s"s${indexes(c)}"
    val rows = nodes.zipWithIndex.map { (c, id) =>
      val extra = c match
        case Const(_, value) => s""", "value":${quote(value.toString)}"""
        case Register(_, cycles) => s""", "cycles":$cycles, "internal":${quote(s"s${indexes(c)+1}")}"""
        case Tap(_, range) => s""", "low":${range.start}, "high":${range.last}"""
        case RAM(_, _, _) => s""", "internal":${quote(s"s${indexes(c)+1}")}"""
        case Times(_, _) => """, "signed":true"""
        case _: Extern | _: SwitchTransposeNetworkComponent =>
          throw IllegalArgumentException("operation contract does not support external arithmetic")
        case _ => ""
      s"""{"id":$id,"op":${quote(c.getClass.getSimpleName)},"width":${c.size},"signal":${quote(name(c))},"inputs":[${c.parents.map(ids).mkString(",")}]$extra}"""
    }
    s"""{"schema":"sgen-lowered-operations-v1","nodes":[${rows.mkString(",")}],"inputs":[${mod.inputs.map(ids).mkString(",")}],"outputs":[${mod.outputs.map(ids).mkString(",")}]}"""
