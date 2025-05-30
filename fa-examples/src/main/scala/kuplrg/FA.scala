package kuplrg

import java.io.{File, PrintWriter}
import io.circe.*, io.circe.syntax.*, io.circe.generic.semiauto.*

/** The definition of finite automaton (FA) */
trait FA extends Acceptable {

  /** The set of states */
  val states: Set[State]

  /** The set of symbols */
  val symbols: Set[Symbol]

  /** The initial state */
  val initState: State

  /** The set of final states */
  val finalStates: Set[State]

  /** The acceptance of a word by DFA
    *
    * @param w
    *   the word to be checked for acceptance
    * @return
    *   `true` if the word is accepted, `false` otherwise
    */
  def accept(w: Word): Boolean

  /** Checks if the finite automaton is valid, or throws an exception */
  lazy val mustValid: this.type = this match
    case dfa: DFA =>
      checkValid(
        symbols,
        dfa.trans,
        !states.contains(_),
      )
    case nfa: NFA =>
      checkValid(
        symbols,
        nfa.trans,
        _.exists(!states.contains(_)),
      )
    case enfa: ENFA =>
      checkValid(
        symbols.map(Some(_)) + None,
        enfa.trans,
        _.exists(!states.contains(_)),
      )

  /** Dumps the finite automaton to a JSON file (viewer/js/data.js) */
  def dump: Unit =
    this.mustValid
    val kind = this.getClass.getSimpleName
    val json = Json.obj(
      "kind" -> kind.toLowerCase.asJson,
      "data" -> this.asJson,
      "mapping" -> Json.obj(),
    )
    val file: File = File("viewer/js/data.js")
    val nf = PrintWriter(file)
    nf.print(s"window.data = ${json.noSpacesSortKeys};")
    nf.close()
    show(s"Dumped the $kind to `viewer/js/data.js`.")
    show("Please open `viewer/index.html` in your browser.")

  // ---------------------------------------------------------------------------
  // Private helper methods
  // ---------------------------------------------------------------------------
  // Checks if the finite automaton is valid
  private def checkValid[Annot, Target](
    annots: Set[Annot],
    trans: Map[(State, Annot), Target],
    invalid: Target => Boolean,
  ): this.type =
    for {
      q <- states
      a <- annots
      b <- trans.get((q, a))
      if invalid(b)
    } error(s"Invalid transition: ($q, $a) -> $b")
    if (!states.contains(initState)) error(s"Invalid initial state: $initState")
    val invalidFinals = finalStates.filter(!states.contains(_))
    if (invalidFinals.nonEmpty)
      error(s"Invalid final states: ${invalidFinals.mkString(", ")}")
    this

  // Implicit conversions and encoders
  private given Conversion[State, String] = _.toString
  private given Conversion[Symbol, String] = _.toString
  private given encodeMap[A, B, V](using
    encodeA: Conversion[A, String],
    encodeB: Conversion[B, String],
    encodeV: Encoder[V],
  ): Encoder[Map[(A, B), V]] = new Encoder {
    final def apply(map: Map[(A, B), V]): Json =
      val pairs = map
        .map {
          case ((a, b), v) => (a.toString, b.toString) -> v
        }
        .toList
        .sortBy(_._1)
      Json.fromFields(pairs.groupBy(_._1._1).map {
        case (a, pairs) =>
          a -> Json.fromFields(pairs.map {
            case ((_, b), v) => b -> encodeV(v)
          })
      })
  }
  private given encodeOptMap[A, B, V](using
    encodeA: Conversion[A, String],
    encodeB: Conversion[B, String],
    encodeV: Encoder[V],
  ): Encoder[Map[(A, Option[B]), V]] = new Encoder {
    final def apply(map: Map[(A, Option[B]), V]): Json =
      val pairs = map
        .map {
          case ((a, b), v) =>
            val bStr = b.map(_.toString).getOrElse("")
            (a.toString, bStr) -> v
        }
        .toList
        .sortBy(_._1)
      Json.fromFields(pairs.groupBy(_._1._1).map {
        case (a, pairs) =>
          a -> Json.fromFields(pairs.map {
            case ((_, b), v) => b -> encodeV(v)
          })
      })
  }
  private given Encoder[DFA] = deriveEncoder[DFA]
  private given Encoder[NFA] = deriveEncoder[NFA]
  private given Encoder[ENFA] = deriveEncoder[ENFA]
  private given Encoder[FA] = Encoder.instance {
    case dfa: DFA   => dfa.asJson
    case nfa: NFA   => nfa.asJson
    case enfa: ENFA => enfa.asJson
  }
}
