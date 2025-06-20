package kuplrg

object Implementation extends Template {

  /** This is the playground for you to run your implementation. Do whatever you
    * want here and run `sbt run` to see the result.
    */
  @main def playground: Unit = {
    println("------------------- PLAYGROUND -------------------")

    // You can check your implementation here.
    val accept = pda_a2n1_b3n2_empty.acceptByFinalState
//    println(s"pda_an_bn_final.acceptByFinalState(\"ab\")   = ${accept("ab")}")
//    println(s"pda_an_bn_final.acceptByFinalState(\"aba\")  = ${accept("aba")}")
//    println(s"pda_an_bn_final.acceptByFinalState(\"aabb\") = ${accept("aabb")}")
    
    println(s"pda_a2n1_b3n2_empty.accepthByEmptyState(\"aaaaabbbbbbbb\") = ${accept("aaaaabbbbbbbb")}")

    println("--------------------------------------------------")
  }

  // PDA accepting L = { a^n b^n | n >= 0 } by final states
  val pda_an_bn_final: PDA = PDA(
    states = Set(0, 1, 2),
    symbols = Set('a', 'b'),
    alphabets = Set("X", "Z"),
    trans = Map(
      (0, Some('a'), "Z") -> Set((0, List("X", "Z"))),
      (0, Some('a'), "X") -> Set((0, List("X", "X"))),
      (0, None, "Z") -> Set((1, List("Z"))),
      (0, None, "X") -> Set((1, List("X"))),
      (1, Some('b'), "X") -> Set((1, List())),
      (1, None, "Z") -> Set((2, List("Z"))),
    ).withDefaultValue(Set()),
    initState = 0,
    initAlphabet = "Z",
    finalStates = Set(2),
  )

  // PDA accepting L = { w \in {0, 1}* | w = w^R and |w| is even }
  // by final states
  def pda_even_pal_final: PDA = PDA (
    states = Set(0, 1, 2),
    symbols = Set('0', '1'),
    alphabets = Set("X", "Y", "Z"),
    trans = Map (
      (0, Some('0'), "Z") -> Set((0, List("X", "Z"))),
      (0, Some('0'), "Y") -> Set((0, List("X", "Y"))),
      (0, Some('0'), "X") -> Set((0, List("X", "X"))),
      (0, Some('1'), "Z") -> Set((0, List("Y", "Z"))),
      (0, Some('1'), "Y") -> Set((0, List("Y", "Y"))),
      (0, Some('1'), "X") -> Set((0, List("Y", "X"))),
      (0, None, "Z") -> Set((1, List("Z"))),
      (0, None, "Y") -> Set((1, List("Y"))),
      (0, None, "X") -> Set((1, List("X"))),
      (1, Some('0'), "X") -> Set((1, List())),
      (1, Some('1'), "Y") -> Set((1, List())),
      (1, None, "Z") -> Set((2, List())),
    ).withDefaultValue(Set()),
    initState = 0,
    initAlphabet = "Z",
    finalStates = Set(2),
  )

  // PDA accepting L = { w \in {a, b}* | N_a(w) <= N_b(w) } by empty stacks
  def pda_more_bs_empty: PDA = PDA (
    states = Set(0, 1),
    symbols = Set('a', 'b'),
    alphabets = Set("Z", "A", "B"),
    trans = Map (
      (0, Some('a'), "Z") -> Set((0, List("A", "Z"))),
      (0, Some('a'), "A") -> Set((0, List("A", "A"))),
      (0, Some('a'), "B") -> Set((0, List())),
      (0, Some('b'), "Z") -> Set((0, List("B", "Z"))),
      (0, Some('b'), "A") -> Set((0, List())),
      (0, Some('b'), "B") -> Set((0, List("B", "B"))),
      (0, None, "Z") -> Set((1, List("Z"))),
      (0, None, "B") -> Set((1, List("B"))),
      (1, None, "B") -> Set((1, List())),
      (1, None, "Z") -> Set((1, List())),
    ).withDefaultValue(Set()),
    initState = 0,
    initAlphabet = "Z",
    finalStates = Set(0, 1),
  )

  // PDA accepting L = { a^i b^j c^k | i, j, k >= 0 and (i = j or j = k) } by
  // final states
  def pda_abc_ij_jk_final: PDA = PDA (
    states = Set(0, 1, 2, 3, 4, 5, 6, 7),
    symbols = Set('a', 'b', 'c'),
    alphabets = Set("Z", "A", "B", "C"),
    trans = Map (
      (0, None, "Z") -> Set(
        (1, List("Z")),
        (4, List("Z"))
      ),

      (1, Some('a'), "Z") -> Set((1, List("A", "Z"))),
      (1, Some('a'), "A") -> Set((1, List("A", "A"))),
      (1, None, "Z") -> Set((2, List("Z"))),
      (1, None, "A") -> Set((2, List("A"))),

      (2, Some('b'), "A") -> Set((2, List())),
      (2, None, "Z") -> Set((3, List("Z"))),

      (3, Some('c'), "Z") -> Set((3, List("Z"))),

      (4, Some('a'), "Z") -> Set((4, List("Z"))),
      (4, None, "Z") -> Set((5, List("Z"))),

      (5, Some('b'), "Z") -> Set((5, List("B", "Z"))),
      (5, Some('b'), "B") -> Set((5, List("B", "B"))),
      (5, None, "Z") -> Set((6, List("Z"))),
      (5, None, "B") -> Set((6, List("B"))),

      (6, Some('c'), "B") -> Set((6, List())),
      (6, None, "Z") -> Set((7, List("Z"))),
    ).withDefaultValue(Set()),
    initState = 0,
    initAlphabet = "Z",
    finalStates = Set(3, 7),
  )

  // PDA accepting L = { a^i b^j | (i = 2n + 1 and j = 3n + 2) for n >= 0 } by
  // empty stacks
  def pda_a2n1_b3n2_empty: PDA = PDA(
    states = Set(0, 1, 2, 3, 4, 5, 6, 7),
    symbols = Set('a', 'b'),
    alphabets = Set("Z", "A", "B"),
    trans = Map(
      (0, Some('a'), "Z") -> Set((1, List("Z"))),
      
      (1, Some('a'), "Z") -> Set((2, List("A", "Z"))),
      (1, Some('a'), "A") -> Set((2, List("A", "A"))),
      (1, Some('b'), "Z") -> Set((3, List("Z"))),
      (1, Some('b'), "A") -> Set((3, List("A"))),
      
      (2, Some('a'), "A") -> Set((1, List("A"))),
      
      (3, Some('b'), "Z") -> Set((4, List("Z"))),
      (3, Some('b'), "A") -> Set((4, List("A"))),
      
      (4, None, "Z") -> Set((7, List())),
      (4, Some('b'), "A") -> Set((5, List("A"))),
      
      (5, Some('b'), "A") -> Set((6, List("A"))),
      (6, Some('b'), "A") -> Set((4, List())),
    ).withDefaultValue(Set()),
    initState = 0,
    initAlphabet = "Z",
    finalStates = Set(0, 1, 2, 3, 4, 5, 6, 7),
  )

  // PDA accepting L = { a^i b^j c^k | i, j, k >= 0 and j = i + 2k } by final
  // states
  def pda_abc_j_i2k_final: PDA = PDA(
    states = Set(0, 1, 2, 3, 4),
    symbols = Set('a', 'b', 'c'),
    alphabets = Set("Z", "A", "B"),
    trans = Map(
      (0, Some('a'), "Z") -> Set((0, List("A", "Z"))),
      (0, Some('a'), "A") -> Set((0, List("A", "A"))),
      (0, None, "Z") -> Set((1, List("Z"))),
      (0, None, "A") -> Set((1, List("A"))),
      
      (1, Some('b'), "B") -> Set((4, List("B", "B"))),
      (1, Some('b'), "Z") -> Set((4, List("B", "Z"))),
      (1, Some('b'), "A") -> Set((1, List())),
      (1, None, "Z") -> Set((2, List("Z"))),
      (1, None, "B") -> Set((2, List("B"))),
      
      (4, Some('b'), "B") -> Set((1, List("B"))),
      
      (2, Some('c'), "B") -> Set((2, List())),
      (2, None, "Z") -> Set((3, List("Z"))),
    ).withDefaultValue(Set()),
    initState = 0,
    initAlphabet = "Z",
    finalStates = Set(3),
  )

  // PDA accepting L = { w \in { '(', ')', '{', '}', '[', ']' }* | w is
  // well-formed and satisfies the order: '()' <= '{}' <= '[]' } by empty stacks
  def pda_ord_brace_empty: PDA = PDA(
    states = Set(0),
    symbols = Set('(', ')', '{', '}', '[', ']'),
    alphabets = Set("Z", "(", ")", "{", "}", "[", "]"),
    trans = Map(
      (0, None, "Z") -> Set((0, List())),
      (0, Some('('), "Z") -> Set((0, List("(", "Z"))),
      (0, Some('('), "(") -> Set((0, List("(", "("))),
      (0, Some('('), "{") -> Set((0, List("(", "{"))),
      (0, Some('('), "[") -> Set((0, List("(", "["))),

      (0, Some(')'), "(") -> Set((0, List())),

      (0, Some('{'), "Z") -> Set((0, List("{", "Z"))),
      (0, Some('{'), "{") -> Set((0, List("{", "{"))),
      (0, Some('{'), "[") -> Set((0, List("{", "["))),

      (0, Some('}'), "{") -> Set((0, List())),

      (0, Some('['), "Z") -> Set((0, List("[", "Z"))),
      (0, Some('['), "[") -> Set((0, List("[", "["))),

      (0, Some(']'), "[") -> Set((0, List())),
    ).withDefaultValue(Set()),
    initState = 0,
    initAlphabet = "Z",
    finalStates = Set(0),
  )

  // PDA accepting L = { a_1 a_2 ... a_2n \in {a, b}* | n >= 1 and a_i = a_{n+i}
  // for some 1 <= i <= n } by final states
  def pda_eq_pair_final: PDA = PDA(
    states = Set(0, 1, 2, 3, 4, 5, 6, 7),
    symbols = Set('a', 'b'),
    alphabets = Set("Z", "X"),
    trans = Map(
      (0, Some('a'), "Z") -> Set((0, List("X", "Z")), (1, List("Z"))),
      (0, Some('a'), "X") -> Set((0, List("X", "X")), (1, List("X"))),
      (0, Some('b'), "Z") -> Set((0, List("X", "Z")), (4, List("Z"))),
      (0, Some('b'), "X") -> Set((0, List("X", "X")), (4, List("X"))),

      (1, Some('a'), "X") -> Set((1, List())),
      (1, Some('b'), "X") -> Set((1, List())),
      (1, None, "Z") -> Set((2, List("Z"))),

      (2, Some('a'), "Z") -> Set((2, List("X", "Z")), (3, List("Z"))),
      (2, Some('a'), "X") -> Set((2, List("X", "X")), (3, List("X"))),
      (2, Some('b'), "Z") -> Set((2, List("X", "Z"))),
      (2, Some('b'), "X") -> Set((2, List("X", "X"))),

      (3, Some('a'), "X") -> Set((3, List())),
      (3, Some('b'), "X") -> Set((3, List())),
      (3, None, "Z") -> Set((6, List("Z"))),

      (4, Some('a'), "X") -> Set((4, List())),
      (4, Some('b'), "X") -> Set((4, List())),
      (4, None, "Z") -> Set((5, List("Z"))),

      (5, Some('a'), "Z") -> Set((5, List("X", "Z"))),
      (5, Some('a'), "X") -> Set((5, List("X", "X"))),
      (5, Some('b'), "Z") -> Set((5, List("X", "Z")), (6, List("Z"))),
      (5, Some('b'), "X") -> Set((5, List("X", "X")), (6, List("X"))),

      (6, Some('a'), "X") -> Set((6, List())),
      (6, Some('b'), "X") -> Set((6, List())),
      (6, None, "Z") -> Set((7, List("Z"))),
    ).withDefaultValue(Set()), 
    initState = 0,
    initAlphabet = "Z",
    finalStates = Set(7)
//    states = Set(0, 1, 2, 3, 4, 5, 6, 7),
//    symbols = Set('a', 'b'),
//    alphabets = Set("Z", "X"),
//    trans = Map(
//      (0, Some('a'), "Z") -> Set((0, List("X", "Z")), (1, List("Z"))),
//      (0, Some('b'), "Z") -> Set((0, List("X", "Z")), (4, List("Z"))),
//      (0, Some('a'), "X") -> Set((0, List("X", "X")), (1, List("X"))),
//      (0, Some('b'), "X") -> Set((0, List("X", "X")), (4, List("X"))),
//
//      (1, Some('a'), "X") -> Set((1, List())),
//      (1, Some('b'), "X") -> Set((1, List())),
//      (1, None, "Z") -> Set((2, List("Z"))),
//
//      (2, Some('a'), "Z") -> Set((2, List("X", "Z")), (3, List("Z"))),
//      (2, Some('a'), "X") -> Set((2, List("X", "X")), (3, List("X"))),
//      (2, Some('b'), "Z") -> Set((2, List("X", "Z"))),
//      (2, Some('b'), "X") -> Set((2, List("X", "X"))),
//
//      (3, Some('a'), "X") -> Set((3, List())),
//      (3, Some('b'), "X") -> Set((3, List())),
//      (3, None, "Z") -> Set((7, List("Z"))),
//
//      (4, Some('a'), "X") -> Set((4, List())),
//      (4, Some('b'), "X") -> Set((4, List())),
//      (4, None, "Z") -> Set((5, List("Z"))),
//
//      (5, Some('a'), "Z") -> Set((5, List("X", "Z"))),
//      (5, Some('a'), "X") -> Set((5, List("X", "X"))),
//      (5, Some('b'), "Z") -> Set((5, List("X", "Z")), (3, List("Z"))),
//      (5, Some('b'), "X") -> Set((5, List("X", "X")), (3, List("X"))),
//      ).withDefaultValue(Set()),
//    initState = 0,
//    initAlphabet = "Z",
//    finalStates = Set(7),
  )

  // PDA accepting L = { x$y | x, y \in {0, 1}* and N(x) + 1 = N(y^R) }
  // where N(w) is the natural number represented by w in binary
  def pda_inc_empty: PDA = PDA(
    states = Set(-1, 0, 1, 2),
    symbols = Set('0', '1', '$'),
    alphabets = Set("Z", "X", "Y"),
    trans = Map(
      (-1, Some('0'), "Z") -> Set((-1, List("Z"))),
      (-1, None, "Z") -> Set((0, List("Z"))),
      
      (0, Some('0'), "Z") -> Set((0, List("X", "Z"))),
      (0, Some('0'), "X") -> Set((0, List("X", "X"))),
      (0, Some('0'), "Y") -> Set((0, List("X", "Y"))),
      (0, Some('1'), "Z") -> Set((0, List("Y", "Z"))),
      (0, Some('1'), "X") -> Set((0, List("Y", "X"))),
      (0, Some('1'), "Y") -> Set((0, List("Y", "Y"))),

      (0, Some('$'), "Z") -> Set((1, List("Z"))),
      (0, Some('$'), "X") -> Set((1, List("X"))),
      (0, Some('$'), "Y") -> Set((1, List("Y"))),

      (1, Some('0'), "Y") -> Set((1, List())),

      (1, Some('1'), "X") -> Set((2, List())),
      (1, Some('1'), "Z") -> Set((2, List("Z"))),

      (2, Some('1'), "Y") -> Set((2, List())),
      (2, Some('0'), "X") -> Set((2, List())),
      (2, Some('0'), "Z") -> Set((2, List("Z"))),
      (2, None, "Z") -> Set((2, List())),
    ).withDefaultValue(Set()),
    initState = -1,
    initAlphabet = "Z",
    finalStates = Set(-1, 0, 1, 2)
//    states = Set(-1, 0, 1, 2), symbols = Set('0', '1', '$'),
//    alphabets = Set("Z", "0", "1"),
//    trans = Map(
//      (-1, Some('0'), "Z") -> Set((-1, List("Z"))),
//      (-1, None, "Z") -> Set((0, List("Z"))),
//      (0, Some('1'), "Z") -> Set((0, List("1", "Z"))),
//      (0, Some('1'), "1") -> Set((0, List("1", "1"))),
//      (0, Some('1'), "0") -> Set((0, List("1", "0"))),
//      (0, Some('0'), "Z") -> Set((0, List("0", "Z"))),
//      (0, Some('0'), "1") -> Set((0, List("0", "1"))),
//      (0, Some('0'), "0") -> Set((0, List("0", "0"))),
//      (0, Some('$'), "Z") -> Set((1, List("Z"))),
//      (0, Some('$'), "0") -> Set((1, List("0"))),
//      (0, Some('$'), "1") -> Set((1, List("1"))),
//
//      (1, Some('0'), "1") -> Set((1, List())),
//      (1, Some('1'), "Z") -> Set((2, List("Z"))),
//      (1, Some('1'), "0") -> Set((2, List())),
//
//      (2, Some('0'), "Z") -> Set((2, List("Z"))),
//      (2, Some('0'), "0") -> Set((2, List())),
//      (2, Some('1'), "1") -> Set((2, List())),
//      (2, None, "Z") -> Set((2, List())),
//    ).withDefaultValue(Set()),
//    initState = -1,
//    initAlphabet = "Z",
//    finalStates = Set(0, 1, 2),
  )

}
