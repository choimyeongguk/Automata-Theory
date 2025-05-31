package kuplrg

import kuplrg.CFG.{rhs, symbol}

object Implementation extends Template {

  /** This is the playground for you to run your implementation. Do whatever you
    * want here and run `sbt run` to see the result.
    */
  @main def playground: Unit = {
    println("------------------- PLAYGROUND -------------------")

    // You can check your implementation here.

    println("--------------------------------------------------")
  }

  // Convert a PDA with final states to a PDA with empty stacks
  def pdafs2es(pda: PDA): PDA = {
    // 기존 PDA 값들 불러오기
    val Q: Set[State]        = pda.states
    val Sigma: Set[Symbol]   = pda.symbols
    val Gamma: Set[Alphabet] = pda.alphabets
    val delta: Map[(State, Option[Symbol], Alphabet), Set[(State, List[Alphabet])]]
                             = pda.trans
    val q0: State            = pda.initState
    val Z: Alphabet          = pda.initAlphabet
    val F: Set[State]        = pda.finalStates

    // 스택 비우기용 새로운 상태 추가
    val newStart: State = Q.max + 1
    val trashCan: State = Q.max + 2   // 가장 큰 상태보다 1 큰 값. 즉 새로운 state
    val Zp: Alphabet = "Z'"

    val Q_new: Set[State] = Q + newStart + trashCan
    val Gamma_new: Set[Alphabet] = Gamma + Zp

    // from new start state to original start state
    val newStart2start: Map[(State, Option[Symbol], Alphabet), Set[(State, List[Alphabet])]] =
      Map((newStart, None, Zp) -> Set((q0, List(Z, Zp))))

    // from all final state to trashCan, eps 이동하는 trans 생성
    val final2trashCan: Map[(State, Option[Symbol], Alphabet), Set[(State, List[Alphabet])]] =
      (for {
        f <- F
        A <- Gamma_new
      } yield {
        // key: (f, eps, A), value: Set((trashCan, List(A)))
        (f, None, A) -> Set((trashCan, List()))
      }).toMap

    val emptyTrashCan: Map[(State, Option[Symbol], Alphabet), Set[(State, List[Alphabet])]] =
      (for {
        A <- Gamma_new
      } yield {
        (trashCan, None, A) -> Set((trashCan, List()))
      }).toMap

    // 기존 전이에 스택 비우기 전이 추가
    val delta_new = delta ++ newStart2start ++ final2trashCan ++ emptyTrashCan

    // 최종적으로 empty-stack PDA 반환 (finalStates는 더 이상 사용하지 않으므로 비워둠)
    PDA(
      states       = Q_new,
      symbols      = Sigma,
      alphabets    = Gamma_new,
      trans        = delta_new.withDefaultValue(Set()),
      initState    = newStart,
      initAlphabet = Zp,
      finalStates  = Set.empty[State]
    )
  }



  // Convert a PDA with empty stacks to a PDA with final states
  def pdaes2fs(pda: PDA): PDA = {
    // 기존 PDA 값들 불러오기
    val Q: Set[State]        = pda.states
    val Sigma: Set[Symbol]   = pda.symbols
    val Gamma: Set[Alphabet] = pda.alphabets
    val delta: Map[(State, Option[Symbol], Alphabet), Set[(State, List[Alphabet])]]
    = pda.trans
    val q0: State            = pda.initState
    val Z: Alphabet          = pda.initAlphabet
    val F: Set[State]        = pda.finalStates

    // start state, final state 역할 하는 새로운 상태 추가
    val startState: State = Q.max + 1   // 가장 큰 상태보다 큰 값. 즉 새로운 state
    val finalState: State = Q.max + 2
    val Zp: Alphabet      = "Z'"  // 설마 Alphabet 중에 Z'이 있는건 아니겠지~~? 그럼 반칙이지

    val Q_new: Set[State]        = Q + startState + finalState
    val Gamma_new: Set[Alphabet] = Gamma + Zp
    val q0_new: State            = startState
    val Z_new: Alphabet          = Zp
    val F_new: Set[State]        = Set(finalState)

    // from new start state to original start state, eps 이동하는 trans 생성
    val start2start: Map[(State, Option[Symbol], Alphabet), Set[(State, List[Alphabet])]] =
      Map((startState, None, Zp) -> Set((q0, List(Z, Zp))))

    // from all states to final state, eps 이동하는 trans 생성
    val all2final: Map[(State, Option[Symbol], Alphabet), Set[(State, List[Alphabet])]] =
      (for {
        q <- Q
      } yield {
        (q, None, Zp) -> Set((finalState, List(Zp)))
      }).toMap

    // 기존 전이에 추가
    val delta_new: Map[(State, Option[Symbol], Alphabet), Set[(State, List[Alphabet])]] =
      delta ++ start2start ++ all2final


    // 최종적으로 final-state PDA 반환
    PDA(
      states       = Q_new,
      symbols      = Sigma,
      alphabets    = Gamma_new,
      trans        = delta_new.withDefaultValue(Set()),
      initState    = q0_new,
      initAlphabet = Z_new,
      finalStates  = F_new
    )
  }

  // Convert a CFG to a PDA with empty stacks
  def cfg2pdaes(cfg: CFG): PDA = {
    // Nt : String
    val nts: Set[Nt]              = cfg.nts
    val symbols: Set[Symbol]      = cfg.symbols
    val start: Nt                 = cfg.start
    val rules: Map[Nt, List[Rhs]] = cfg.rules

    val q: State = 0                             // 필요한 state는 하나면 충분!

    val Q: Set[State]        = Set(q)            // state 집합
    val Sigma: Set[Symbol]   = symbols           // symbol(terminal) 집합
    val Gamma: Set[Alphabet] = nts ++ symbols.map(_.toString)  // terminal과 non-terminal 모두 스택에 들어갈 수 있음
    val q0: State            = q                 // 유일한 state가 시작 state
    val Z: Alphabet          = cfg.start         // 초기 alphbet은 CFG의 start non-terminal
    val F: Set[State]        = Set.empty[State]  // PDAes이기 때문에 final state 불필요

    // terminal을 지워주는 consuming trans 생성
    val symbol2eps: Map[(State, Option[Symbol], Alphabet), Set[(State, List[Alphabet])]] =
      (for {
        s <- symbols
      } yield {
        (q, Some(s), s.toString) -> Set((q, List()))
      }).toMap

    // non-terminal을 변환하는 eps trans 생성
    val nt2rhs: Map[(State, Option[Symbol], Alphabet), Set[(State, List[Alphabet])]] =
      (for {
        (nt, rhsList) <- rules.toSeq    // key: Nt, rhsList: List[Rhs]
        rhs           <- rhsList
      } yield {
        // List[Nt | Symbol] -> List[Alphabet] 형태로 변환
        val alphaRhs: List[Alphabet] = rhs.seq.map {
          case nt: Nt    => nt
          case t: Symbol => t.toString  // symbol은 alphabet에 맞게 string으로 바꿔줌
        }
        (q, None, nt) -> Set((q, alphaRhs))
      })
      .groupMap(_._1)(_._2)   // key를 기준으로 일단 그룹핑한 후, value 값을 모아서 리스트로 만듦
      .view                   // toMap 전까지 값을 실제로 변환하지 않고 변환작업만 쌓아둠
      .mapValues(_.reduce(_ ++ _))  // List 안의 Set을 합집합 연산해서 하나로 합쳐줌
      .toMap

    val delta = symbol2eps ++ nt2rhs

    PDA(
      states       = Q,
      symbols      = Sigma,
      alphabets    = Gamma,
      trans        = delta.withDefaultValue(Set()),
      initState    = q0,
      initAlphabet = Z,
      finalStates  = F
    )
  }

  // -> 여기까진 참 쉬운데...

  // Convert a PDA with empty stacks to a CFG
  def pdaes2cfg(pda: PDA): CFG = {
    // 너무 어렵다...
    val Q     = pda.states.toList
    val Sigma = pda.symbols
    val q0    = pda.initState
    val Z0    = pda.initAlphabet
    val S: Nt = "S"

    // non-terminal AijX 문자열 생성
    // AijX : i상태에서, X를 소모하여 j상태까지 가는데 생성할 수 있는 모든 문자열
    //        이때, 스택에 새로운 알파벳이 추가되거나 X이외의 기존의 문자열이 제거되어서는 안된다!!!!
    //        즉, X하나를 소모하여 만들어낼 수 있는 문자열 전부 표현!
    // ApqX -> a Ars1Y1 As1s2Y2 ... Ask-1qYk
    // "p상태에서 X를 꺼내면서 a를 읽는다" -> a를 Rhs의 가장 앞에 두면 일단 a를 성공적으로 먼저 읽을 수 있음
    // 그다음 X 밑의 알파벳의 차례가 오기 위해 새로이 추가된 알파벳들을 소모해야 하는데 위의 각각의 non-terminal 들은
    // ApqX가 스택에서 각 알파벳을 꺼내면서 거치는 경로들을 대신 표현해준다.
    def nt(p: State, q: State, x: Alphabet): Nt =
      s"A_${p}_${q}_${x}"

    // |Q|^n 개의 조합 생성
    def tuples(n: Int): List[List[State]] =
      if n == 0 then List(Nil)
      else for {
        s <- Q; rest <- tuples(n - 1)
      } yield s :: rest

    // 시작state에서 다른 모든 state로 가는 생성규칙
    val S2all: List[(Nt, Rhs)] = Q.map(q => S -> Rhs(List(nt(q0, q, Z0))))

    // non-terminal의 생성규칙
    val all2all: List[(Nt, Rhs)] =
      (for {
        // PDA trans 분해
        // 일단 trans 규칙에 의해 p에서 x를 소모해 r까지 가면, 스택에는 gamma가 쌓이게 된다.
        // 이 gamma를 전부 소모하여 다른 상태로 가는 모든 경우를 계산해야 한다.
        // gamma를 소모하는 과정에서 경유하는 상태는 자유롭게 선택할 수 있기 때문에
        // r -> k1 -> k2 -> k3 -> ... -> km -> q 와 같은 경로에서
        // m == |gamma|-1 이다. 왜냐하면 한 번 이동하는 데 알파벳을 하나 씩 소비해야하기 때문에,
        // (gamma의 길이)번 만큼 상태를 옮길 수 있다. 최종 상태는 q여야 하기 때문에 중간다리 상태는 |gamma|-1 개 있다.
        // 예를 들어 |gamma|==3 이라면, r -> k1 -> k2 -> q 와 같이 이동할 수 있다.
        ((p, a, x), targets) <- pda.trans.toList      // 현재state, symbol, pop되는 alphabet
        (r, gamma) <- targets.toList                  // 임시도착state, push되는 alphabets
      } yield {
        val aPart = a.toList                          // Some(a)→List(a), None→Nil
        if (gamma.isEmpty) {                          // pop만 함. 경유할 수 없음
          List(nt(p, r, x) -> Rhs(aPart))             // q == r 로 고정
        }
        else {                                        // push하는 non-terminal 존재
          val k = gamma.length                        // k == 새로 push하는 non-terminal의 개수
          for {
            middle <- tuples(k - 1)                   // 1~|Q| 범위 (k-1)개 숫자의 중복순열. 즉 경유하는 상태 개수
            q      <- Q                               // 모든 경유의 최종 상태 q에 대해
            path = (r :: middle) :+ q                 // 임시도착상태인 r에서 시작해 경유 상태를 거쳐 최종적으로 q에 도달하는 경로
            rhsNTs = gamma.indices.toList.map(i =>    // gamma의 각 알파벳 소비 단계에서 어떤 경로를 거쳤는지 기록
              nt(path(i), path(i + 1), gamma(i)))
          } yield nt(p, q, x) -> Rhs(aPart ++ rhsNTs) // symbol + non-terminal 조합의 생성규칙 완성!!
        }
      }).flatten // (List[List]) → List

    // (Nt, Rhs) 형태로 계산한 생성규칙을 Map으로 변환 + 중복 Rhs 제거
    val explicitRules: Map[Nt, List[Rhs]] =
      (S2all ++ all2all)
        .groupBy(_._1)                   // Nt를 기준으로 그룹핑. 즉 Nt가 같으면 일단 모아줌
                                         // List[(Nt, Rhs)] -> Map[Nt, List[(Nt, Rhs)]]
        .view                            // Map[Nt, List[(Nt, Rhs)]] -> MapView[Nt, List[(Nt, Rhs)], Map[Nt, List[(Nt, Rhs)]]]
        .mapValues(_.map(_._2).distinct) // (Nt,Rhs) 튜플에서 Rhs만 뽑아서 List로 만듦 + 중복 제거
        .toMap                           // 최종 Map[Nt, List[Rhs]] 형태 완성

    // 계산되지 않은 non-terminal은 빈 리스트로 채우기
    val referencedNTs: Set[Nt] =
      (S2all ++ all2all)
        // 모든 생성규칙에서 non-terminal만 뽑아냄
        .flatMap { case (_, Rhs(seq)) => seq.collect { case nt: Nt => nt } }
        .toSet  // 중복 제거하여 Rhs에 있는 모든 Nt Set 완성

    // Rhs Nt + Lhs Nt + S
    val allNTs: Set[Nt] = referencedNTs ++ explicitRules.keySet + S

    // 명시적 생성 규칙 존재하면 그 규칙을 따르고, 없으면 빈 리스트로 채워주기
    val rules: Map[Nt, List[Rhs]] =
      allNTs.map(nt => nt -> explicitRules.getOrElse(nt, Nil)).toMap

    CFG(
      nts = allNTs,
      symbols = Sigma,
      start = S,
      rules = rules
    )
  }


}
