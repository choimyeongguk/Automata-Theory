package kuplrg

object Implementation extends Template {

  /** This is the playground for you to run your implementation. Do whatever you
    * want here and run `sbt run` to see the result.
    */
  @main def playground: Unit = {
    println("------------------- PLAYGROUND -------------------")

    // You can check your implementation here.
    println("dfa_waa.accept(aa)  = " + dfa_waa.accept("aa"))
    println("dfa_waa.accept(ab)  = " + dfa_waa.accept("ab"))
    println("dfa_waa.accept(baa) = " + dfa_waa.accept("baa"))

    println("--------------------------------------------------")

    // You can dump the DFA to see it in the automaton viewer.
    // After running this program, open `viewer/index.html` in your browser.
    enfa_complex.dump

    println("--------------------------------------------------")
  }

  // ---------------------------------------------------------------------------
  // Deterministic Finite Automata (DFA)
  // ---------------------------------------------------------------------------
  // Example DFA for L = { w a a | w \in {a, b}* }
  def dfa_waa: DFA = DFA(
    states = Set(0, 1, 2),
    symbols = Set('a', 'b'),
    trans = Map(
      (0, 'a') -> 1,
      (0, 'b') -> 0,
      (1, 'a') -> 2,
      (1, 'b') -> 0,
      (2, 'a') -> 2,
      (2, 'b') -> 0,
    ),
    initState = 0,
    finalStates = Set(2),
  )

  // (Problem #1) DFA for L = { a b^n | n >= 1 }
  def dfa_a_b_plus: DFA = DFA(
    states = Set(0, 1, 2, 3),
    symbols = Set('a', 'b'),
    trans = Map(
      (0, 'a') -> 1, (0, 'b') -> 3,
      (1, 'a') -> 3, (1, 'b') -> 2,
      (2, 'a') -> 3, (2, 'b') -> 2,
      (3, 'a') -> 3, (3, 'b') -> 3,
    ),
    initState = 0,
    finalStates = Set(2),
  )

  // (Problem #2) DFA for L = { w \in {0, 1}* | N(w) \equiv 2 (mod 3) }
  // where N(w) is the natural number represented by w in binary
  def dfa_div_3_2: DFA = DFA(
    states = Set(0, 1, 2),
    symbols = Set('0', '1'),
    trans = Map(
      (0, '0') -> 0, (0, '1') -> 1,
      (1, '0') -> 2, (1, '1') -> 0,
      (2, '0') -> 1, (2, '1') -> 2,
    ),
    initState = 0,
    finalStates = Set(2),
  )

  // (Problem #3) DFA for L = { w \in {0, 1}* | w contains a subsequence 011 }
  def dfa_subseq_011: DFA = DFA(
    states = Set(0, 1, 2, 3),
    symbols = Set('0', '1'),
    trans = Map(
      (0, '0') -> 1, (0, '1') -> 0,
      (1, '0') -> 1, (1, '1') -> 2,
      (2, '0') -> 2, (2, '1') -> 3,
      (3, '0') -> 3, (3, '1') -> 3,
    ),
    initState = 0,
    finalStates = Set(3),
  )

  // (Problem #4) DFA for L = { w \in {0, 1}* | w contains even number of 0's
  // and even number of 1's }
  def dfa_even_0_1: DFA = DFA(
    states = Set(0, 1, 2, 3),
    symbols = Set('0', '1'),
    trans = Map(
      (0, '0') -> 2, (0, '1') -> 1,
      (1, '0') -> 3, (1, '1') -> 0,
      (2, '0') -> 0, (2, '1') -> 3,
      (3, '0') -> 1, (3, '1') -> 2,
    ),
    initState = 0,
    finalStates = Set(0),
  )

  // ---------------------------------------------------------------------------
  // Nondeterministic Finite Automata (NFA)
  // ---------------------------------------------------------------------------
  // Example NFA for L = { w \in {0, 1}* | w contains at least two 0's }
  def nfa_least_two_0: NFA = NFA(
    states = Set(0, 1, 2),
    symbols = Set('0', '1'),
    trans = Map(
      (0, '0') -> Set(0, 1),
      (0, '1') -> Set(0),
      (1, '0') -> Set(1, 2),
      (1, '1') -> Set(1),
      (2, '0') -> Set(2),
      (2, '1') -> Set(2),
    ).withDefaultValue(Set()),
    initState = 0,
    finalStates = Set(2),
  )

  // (Problem #5) NFA for L = { w \in {0, 1}* | w contains at most three 0's }
  def nfa_most_three_0: NFA = NFA(
    states = Set(0, 1, 2, 3, 4),
    symbols = Set('0', '1'),
    trans = Map(
      (0, '0') -> Set(1), (0, '1') -> Set(0),
      (1, '0') -> Set(2), (1, '1') -> Set(1),
      (2, '0') -> Set(3), (2, '1') -> Set(2),
      (3, '0') -> Set(4), (3, '1') -> Set(3),
    ).withDefaultValue(Set()),
    initState = 0,
    finalStates = Set(0, 1, 2, 3),
  )

  // (Problem #6) NFA for L = { w \in {a, b}* | w does not contain
  // "bab" as a substring }
  def nfa_not_substr_bab: NFA = NFA(
    states = Set(0, 1, 2, 3),
    symbols = Set('a', 'b'),
    trans = Map(
      (0, 'a') -> Set(0), (0, 'b') -> Set(1),
      (1, 'a') -> Set(2), (1, 'b') -> Set(1),
      (2, 'a') -> Set(0), (2, 'b') -> Set(3),
    ).withDefaultValue(Set()),
    initState = 0,
    finalStates = Set(0, 1, 2),
  )

  // (Problem #7) NFA for L = { w \in {a, b}* | w is any combination of "aaa"
  // and "bb" }
  def nfa_comb_aaa_bb: NFA = NFA(
    states = Set(0, 1, 2, 3),
    symbols = Set('a', 'b'),
    trans = Map(
      (0, 'a') -> Set(1), (0, 'b') -> Set(3),
      (1, 'a') -> Set(2),
      (2, 'a') -> Set(0),
      (3, 'b') -> Set(0),
    ).withDefaultValue(Set()),
    initState = 0,
    finalStates = Set(0),
  )

  // ---------------------------------------------------------------------------
  // epsilon-Non-deterministic Finite Automata (epsilon-NFA)
  // ---------------------------------------------------------------------------
  // Example ENFA for L = { a^i b^j c^k | i, j, k >= 0 }
  def enfa_ai_bj_ck: ENFA = ENFA(
    states = Set(0, 1, 2),
    symbols = Set('a', 'b', 'c'),
    trans = Map(
      (0, None)      -> Set(1),
      (0, Some('a')) -> Set(0),
      (1, None)      -> Set(2),
      (1, Some('b')) -> Set(1),
      (2, Some('c')) -> Set(2),
    ),
    initState = 0,
    finalStates = Set(2),
  )

  // (Problem #8) ENFA for L = { (aba)^n | n >= 1 }
  def enfa_aba_plus: ENFA = ENFA(
    states = Set(0, 1, 2, 3),
    symbols = Set('a', 'b'),
    trans = Map(
      (0, Some('a')) -> Set(1),
      (1, Some('b')) -> Set(2),
      (2, Some('a')) -> Set(3),
      (3, None)      -> Set(0),
    ),
    initState = 0,
    finalStates = Set(3),
  )

  // (Problem #9) ENFA for L = { 0^n | n >= 1 } U { 1^n | n >= 1 }
  def enfa_same_digits: ENFA = ENFA(
    states = Set(0, 1, 2),
    symbols = Set('0', '1'),
    trans = Map(
      (0, Some('0')) -> Set(1), (0, Some('1')) -> Set(2),
      (1, Some('0')) -> Set(1),
      (2, Some('1')) -> Set(2),
    ),
    initState = 0,
    finalStates = Set(1, 2),
  )

  // (Problem #10) ENFA for L = L_1 \cap L_2
  // where
  // L_1 = { w \in {0, 1} | N(w) \equiv 1 (mod 3) }
  // L_2 = { w \in {0, 1} | zeros(w) \equiv 1 (mod 3) }
  // N(w) is the natural number represented by w in binary,
  // and zeros(w) is the number of 0's in w
  def enfa_complex: ENFA = ENFA(
    states = Set(0, 1, 2, 3, 4, 5, 6, 7, 8),
    symbols = Set('0', '1'),
    trans = Map(
      (0, Some('0')) -> Set(1), (0, Some('1')) -> Set(3),
      (1, Some('0')) -> Set(2), (1, Some('1')) -> Set(4),
      (2, Some('0')) -> Set(0), (2, Some('1')) -> Set(5),
      (3, Some('0')) -> Set(7), (3, Some('1')) -> Set(0),
      (4, Some('0')) -> Set(8), (4, Some('1')) -> Set(1),
      (5, Some('0')) -> Set(6), (5, Some('1')) -> Set(2),
      (6, Some('0')) -> Set(4), (6, Some('1')) -> Set(6),
      (7, Some('0')) -> Set(5), (7, Some('1')) -> Set(7),
      (8, Some('0')) -> Set(3), (8, Some('1')) -> Set(8),
    ),
    initState = 0,
    finalStates = Set(4),
  )
}
