package kuplrg

object Implementation extends Template {

  import RE.*
  import SFA.Edge
  import Fuzzer.*

  /** This is the playground for you to run your implementation. Do whatever you
    * want here and run `sbt run` to see the result.
    */
  @main def playground: Unit = {
    println("------------------- PLAYGROUND -------------------")

    // example RE
    val re1 = RE("</>")
    val re2 = RE("a|b")
    val re3 = RE("0*")

    // example DFA
    val dfa1 = DFA(1, "01", "0", "1")
    val dfa2 = DFA(2, "01", "3", "2")
    val dfa3 = DFA(2, "ab", "5", "2")

    // example ENFA
    val enfa1 = ENFA(4, "ab", "fs8wsgw", "8")
    val enfa2 = ENFA(6, "ab", "ost0183xsrlx05m", "w")
    val enfa3 = ENFA(6, "01", "urgp7qvlhgmm39c", "w")

    // You can dump an FA to see it in the automaton viewer.
    // After running this program, open `viewer/index.html` in your browser.
    dfa1.dump

    println("--------------------------------------------------")

    // You can dump an RE to see its string form and original form.
    re1.dump

    println("--------------------------------------------------")

    // You can generate random FA or RE to test your implementation.
    //
    // fuzzDFA(n = 3, symbols = "01").dump
    // fuzzENFA(n = 3, symbols = "01").dump
    fuzzRE(depth = 3, symbols = "01").dump

    println("--------------------------------------------------")
  }

  // ---------------------------------------------------------------------------
  // Problem 1: Regular Expressions to epsilon-NFA
  // ---------------------------------------------------------------------------
  /** Convert a regular expression to an epsilon-NFA
    *
    * @param re
    * @return
    *   the equivalent epsilon-NFA
    */
  def reToENFA(re: RE): ENFA = reToSFA(re, 1).toENFA

  /** Convert a regular expression to a simplified epsilon-NFA with a given
    * initial state `i`
    *
    * @param re
    *   the regular expression
    * @param i
    *   the initial state
    * @return
    *   the simplified epsilon-NFA
    */
  def reToSFA(re: RE, i: State): SFA = re match
    // SFA는 Template.scala에 구현되어 있음
    case Emp =>
      SFA(
        // {}는 간선 없음
        from = i,
        edges = Set(),
        to = i + 1,
      )
    case Eps =>
      SFA(
        // eps는 symbol 소모 없음
        from = i,
        edges = Set(Edge(i, None, i + 1)),
        to = i + 1,
      )
    case Sym(symbol) =>
      SFA(
        // symbol은 하나 소모. Option으로 돼있으므로 Some()안에
        from = i,
        edges = Set(Edge(i, Some(symbol), i + 1)),
        to = i + 1,
      )
    case Union(left, right) =>
      // union은 병렬 연결
      val SFA(_, ledges, j) = reToSFA(left, i + 1)  // i+1 부터 시작
      // 시작 state 는 어차피 i+1 로 알고 있으니까 상관x
      // j 에는 l 경로의 마지막 state 저장
      // ledges 에는 l 경로의 edge 들 저장
      val SFA(_, redges, k) = reToSFA(right, j + 1) // j+1 부터 시작
      SFA(
        from = i,
        edges = ledges ++ redges ++ Set(  // Set 연결 연산자
          // l 경로 연결
          Edge(i, None, i + 1),
          Edge(j, None, k + 1),
          // r 경로 연결
          Edge(i, None, j + 1),
          Edge(k, None, k + 1),
        ),
        to = k + 1,
      )
    case Concat(left, right) =>
      // Concat는 병렬 연결
      val SFA(_, ledges, j) = reToSFA(left, i)
      val SFA(_, redges, k) = reToSFA(right, j + 1)
      SFA(
        from = i,
        edges = ledges ++ redges ++ Set(
          Edge(j, None, j + 1)
        ),
        to = k,
      )
    case Star(re)            =>
      // *는 다음으로 직행 또는 re 반복
      val SFA(_, edges, j) = reToSFA(re, i + 1)
      SFA(
        from = i,
        edges = edges ++ Set(
          Edge(i, None, j + 1), // 다음으로 직행
          Edge(i, None, i + 1), // 반복 진입
          Edge(j, None, i + 1), // re 반복
          Edge(j, None, j + 1), // 반복 탈출
        ),
        to = j + 1,
      )

  // ---------------------------------------------------------------------------
  // Problem 2: DFA to Regular Expressions
  // ---------------------------------------------------------------------------
  /** Convert a DFA to a regular expression.
    *
    * @param dfa
    *   the given DFA
    * @return
    *   the equivalent regular expression
    */
  def dfaToRE(dfa: DFA): RE =
    dfa.finalStates
      // reForPaths 함수는 각 노드의 R(n)(i,j)를 구해줌
      // R(n)(i, j) : 1~n 노드의 경유를 허용했을 때, q_i -> q_j 로 가는 re
      // map 메서드로 final state 를 각각의 re 로 mapping (replace)
      .map(Implementation.reForPaths(dfa, dfa.initState, _, dfa.states.size))
      // 각 final state 별 re를 최종적으로 합쳐줌
      .foldLeft(Emp)(Union(_, _))

  /** A regular expression accepting paths from `i` to `j` with intermediate
    * states bounded by `k` in a given DFA `dfa`. Assume that the given DFA
    * `dfa` is already normalized (i.e., the states of DFA are 1, 2, ..., n).
    *
    * @param dfa
    *   the given DFA
    * @param i
    *   the initial state
    * @param j
    *   the final state
    * @param k
    *   the bound of intermediate states
    */

  // Warshall-Floyd 알고리즘 비슷한 방법
  // 가능한 경유 노드를 하나씩 늘려가며 re 구해나감
  def reForPaths(dfa: DFA, i: State, j: State, k: State): RE = k match
    case 0 => // 경유 가능한 노드 개수 0개 : 직행만 인정
      val symbol = dfa.symbols
        // dfs symbol 중에 시작 state 에 적용해서 도착 state 에 도달가능한 symbol 선택
        .filter(s => dfa.trans((i, s)) == j)
        // re 형태로 변환
        .map(Sym(_))
      val re = symbol.foldLeft(Emp)(Union(_, _))  // 결과 합침
      if (i == j) Union(Eps, re)  // 출발=시작 이라면 eps 도 포함됨
      else re
    case _ =>
      val R1 = reForPaths(dfa, i, j, k - 1) // R(k-1)(i,j)  // k 경유하지 않는 경로
      val R2 = reForPaths(dfa, i, k, k - 1) // R(k-1)(i,k)  // i -> k
      val R3 = reForPaths(dfa, k, k, k - 1) // R(k-1)(k,k)  // k -> k 사이클
      val R4 = reForPaths(dfa, k, j, k - 1) // R(k-1)(k,j)  // k -> j
      Union(R1, Concat(R2, Concat(Star(R3), R4))) // R1|R2(R3)*R4


  // ---------------------------------------------------------------------------
  // Problem 3: epsilon-NFA to DFA
  // ---------------------------------------------------------------------------
  /** Convert an epsilon-NFA to a DFA.
    *
    * @param enfa
    *   the epsilon-NFA
    * @return
    *   the equivalent DFA
    */
    // 특정 state 집합 에서 같은 word 를 사용해서 도달가능한 state 집합을 DFA 의 하나의 상태로 설정
  def enfaToDFA(enfa: ENFA): DFA =
    // An auxiliary function to find all reachable set of states in epsilon-NFA
    // using depth-first search, and return the mapping from each of them to the
    // corresponding state in DFA.
    def aux(
      qsList: List[Set[State]], // 도달 가능 state 집합
      k: Int, // dfs state
      map: Map[Set[State], Int],
    ): Map[Set[State], State] = qsList match
      case Nil        => map  // 더이상 갈 곳 없음: map 반환
      case qs :: rest =>
        // The next possible set of states
        val next: Set[Set[State]] =
          enfa.symbols.map(a => enfa.extTrans(qs, a.toString))
        // The set of already visited set of states
        val visited: Set[Set[State]] = map.keySet
        // The set of states
        // 다음에 방문 가능한 상태 집합 중 아직 방문하지 않은 상태 집합
        val yets: Set[Set[State]] = next -- visited - qs
        aux(
          yets.toList ++ rest,
          k + 1,
          map + (qs -> k),  // map 에 enfa's state 집합 -> dfa 상태 삽입
        )
    // 시작 state + 시작 state 에서 무료로 도달 가능한 state 집합
    val init: Set[State] = enfa.eclo(enfa.initState)
    // The mapping from reachable set of states in epsilon-NFA to the
    // corresponding state in DFA
    val map: Map[Set[State], State] = aux(List(init), 1, Map.empty)

    // Construct the resulting DFA
    DFA(
      states     = map.values.toSet,
      symbols    = enfa.symbols,
      trans      =
        (for {
          qs <- map.keys      // 가능한 출발 상태들 순회
          s  <- enfa.symbols  // 가능한 모든 symbol 순회
        } yield ((map(qs), s), map(enfa.extTrans(qs, s.toString)))).toMap,
        // map(qs) : qs 상태 집합에 대응되는 새로운 DFA의 상태,
      initState  = map(init),
      finalStates= map
        .filter { 
          // final state 가 state 집합의 원소인 경우만 남김
          case (qs, _) => qs.intersect(enfa.finalStates).nonEmpty
        }
        .map {
          // map 자료형에서 value 값만 뽑음
          case (_, state) => state
        }
        .toSet,
    )
}
