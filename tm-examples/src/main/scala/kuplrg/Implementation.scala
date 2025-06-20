package kuplrg

object Implementation extends Template {

  /** This is the playground for you to run your implementation. Do whatever you
    * want here and run `sbt run` to see the result.
    */
  @main def playground: Unit = {
    println("------------------- PLAYGROUND -------------------")

    // You can check your implementation here.
//    val accept = tm_an_bn_cn.accept
//    println(s"tm_an_bn_cn.accept(\"abc\")    = ${accept("abc")}")
//    println(s"tm_an_bn_cn.accept(\"aabbcc\") = ${accept("aabbcc")}")
//    println(s"tm_an_bn_cn.accept(\"abcabc\") = ${accept("abcabc")}")

    val accept = tm_square.accept
//    println(s"tm_square.accept(\"a\")    = ${accept("a")}")
//    println(s"tm_square.accept(\"aa\") = ${accept("aa")}")
//    println(s"tm_square.accept(\"a*1000\") = ${accept("a"*1000)}")
//    println(s"tm_square.accept(\"aaa\") = ${accept("aaa")}")
//    println(s"tm_square.accept(\"aaaa\") = ${accept("aaaa")}")
//    println(s"tm_square.accept(\"aaaaa\") = ${accept("aaaaa")}")
//    println(s"tm_square.accept(\"aaaaaa\") = ${accept("aaaaaa")}")
//    println(s"tm_square.accept(\"aaaaaaa\") = ${accept("aaaaaaa")}")
//    println(s"tm_square.accept(\"aaaaaaaa\") = ${accept("aaaaaaaa")}")
//    println(s"tm_square.accept(\"aaaaaaaaa\") = ${accept("aaaaaaaaa")}")
//    println(s"tm_square.accept(\"aaaaaaaaaa\") = ${accept("aaaaaaaaaa")}")
//    println(s"tm_square.accept(\"a^16\") = ${accept("aaaaaaaaaaaaaaaa")}")
//    println(s"tm_square.accept(\"a^17\") = ${accept("aaaaaaaaaaaaaaaaa")}")
//    println(s"tm_square.accept(\"a^25\") = ${accept("aaaaaaaaaaaaaaaaaaaaaaaaa")}")
//    println(s"tm_square.accept(\"a^24\") = ${accept("aaaaaaaaaaaaaaaaaaaaaaaa")}")
//    println(s"tm_square.accept(\"a^26\") = ${accept("aaaaaaaaaaaaaaaaaaaaaaaaaa")}")
//    println(s"tm_square.accept(\"a^0\") = ${accept("")}")
//
    def isPerfectSquare(n: Long): Boolean = {
      def loop(i: Long): Boolean = {
        val square = i * i
        if (square == n) true
        else if (square > n) false
        else loop(i + 1)
      }

      if (n < 0) false else loop(1L)
    }

    for(i<-1 to 10000) {
      if(isPerfectSquare(i)) {  // 제곱수
        if(!accept("a"*i))  {
          println(s"ERRORR!\n")
        }
        else print(s"g ")
      }
      else {
        if(accept("a"*i)) { // 제곱수 아님
          println(s"ERRORRR!\n")
        }
        else print(s"g ")
      }
    }

//    val accept = tm_fib.accept
//    println(s"tm_fib.accept(\"\")    = ${accept("")}")
//    println(s"tm_fib.accept(\"a\") = ${accept("a")}")
//    println(s"ttm_fib.accept(\"aa\") = ${accept("aa")}")
//    println(s"ttm_fib.accept(\"aaa\") = ${accept("aaa")}")
//    println(s"ttm_fib.accept(\"aaaa\") = ${accept("aaaa")}")
//    println(s"ttm_fib.accept(\"aaaaa\") = ${accept("aaaaa")}")
//    println(s"ttm_fib.accept(\"aaaaaa\") = ${accept("aaaaaa")}")
//    println(s"ttm_fib.accept(\"aaaaaaa\") = ${accept("aaaaaaa")}")
//    println(s"ttm_fib.accept(\"a*8\") = ${accept("a"*8)}")
//    println(s"ttm_fib.accept(\"a*9\") = ${accept("a"*9)}")

//    val accept = tm_eq_abc.accept
//    println(s"tm_eq_abc.accept(\"\") = ${accept("")}")
//    println(s"tm_eq_abc.accept(\"cabcbbc\") = ${accept("cabcbbc")}")
//    println(s"tm_eq_abc.accept(\"bcbbacab\") = ${accept("bcbbacab")}")
//    println(s"tm_eq_abc.accept(\"cbcbacbaca\") = ${accept("cbcbacbaca")}")
//    println(s"tm_eq_abc.accept(\"b\") = ${accept("b")}")
//    println(s"tm_eq_abc.accept(\"bbb\") = ${accept("bbb")}")
//    println(s"tm_eq_abc.accept(\"bcbcbcbbbc\") = ${accept("bcbcbcbbbc")}")
//    println(s"tm_eq_abc.accept(\"cbcacbc\") = ${accept("cbcacbc")}")
//    println(s"tm_eq_abc.accept(\"ccbaacbcb\") = ${accept("ccbaacbcb")}")
//    println(s"tm_eq_abc.accept(\"cbccccacc\") = ${accept("cbccccacc")}")
//    println(s"tm_eq_abc.accept(\"cb\") = ${accept("cb")}")
//    println(s"tm_eq_abc.accept(\"baccbbbbccbbb\") = ${accept("baccbbbbccbbb")}")
//    println(s"tm_eq_abc.accept(\"bbbcbbb\") = ${accept("bbbcbbb")}")
//    println(s"tm_eq_abc.accept(\"abcaccbcccab\") = ${accept("abcaccbcccab")}")
//    println(s"tm_eq_abc.accept(\"accacbabbcc\") = ${accept("accacbabbcc")}")
//    println(s"tm_eq_abc.accept(\"baacbccc\") = ${accept("baacbccc")}")
//    println(s"tm_eq_abc.accept(\"abcccabbcbcbbbc\") = ${accept("abcccabbcbcbbbc")}")
//    println(s"tm_eq_abc.accept(\"bbcccbccccca\") = ${accept("bbcccbccccca")}")
//    println(s"tm_eq_abc.accept(\"cbcb\") = ${accept("cbcb")}")
//    println(s"tm_eq_abc.accept(\"cbc\") = ${accept("cbc")}")
//    println(s"tm_eq_abc.accept(\"cbcbcb\") = ${accept("cbcbcb")}")
//    println(s"tm_eq_abc.accept(\"ccbbbba\") = ${accept("ccbbbba")}")
//    println(s"tm_eq_abc.accept(\"bbbabc\") = ${accept("bbbabc")}")
//    println(s"tm_eq_abc.accept(\"cccabb\") = ${accept("cccabb")}")
//    println(s"tm_eq_abc.accept(\"bbcacbbb\") = ${accept("bbcacbbb")}")
//    println(s"tm_eq_abc.accept(\"bbc\") = ${accept("bbc")}")
//    println(s"tm_eq_abc.accept(\"cbc\") = ${accept("cbc")}")
//    println(s"tm_eq_abc.accept(\"baccbbca\") = ${accept("baccbbca")}")
//    println(s"tm_eq_abc.accept(\"bcbaabccac\") = ${accept("bcbaabccac")}")
//    println(s"tm_eq_abc.accept(\"cc\") = ${accept("cc")}")
//    println(s"tm_eq_abc.accept(\"cababbcbbcac\") = ${accept("cababbcbbcac")}")
//    println(s"tm_eq_abc.accept(\"ccbcab\") = ${accept("ccbcab")}")
//    println(s"tm_eq_abc.accept(\"bababcccb\") = ${accept("bababcccb")}")
//    println(s"tm_eq_abc.accept(\"cbcbacbacabcca\") = ${accept("cbcbacbacabcca")}")
//    println(s"tm_eq_abc.accept(\"acbcbbcbb\") = ${accept("acbcbbcbb")}")
//    println(s"tm_eq_abc.accept(\"bbbacabc\") = ${accept("bbbacabc")}")
//    println(s"tm_eq_abc.accept(\"bccc\") = ${accept("bccc")}")
//    println(s"tm_eq_abc.accept(\"abacbbcbbac\") = ${accept("abacbbcbbac")}")
//    println(s"tm_eq_abc.accept(\"bacbcaacbb\") = ${accept("bacbcaacbb")}")
//    println(s"tm_eq_abc.accept(\"bcb\") = ${accept("bcb")}")
//    println(s"tm_eq_abc.accept(\"c\") = ${accept("c")}")
//    println(s"tm_eq_abc.accept(\"bcbb\") = ${accept("bcbb")}")
//    println(s"tm_eq_abc.accept(\"b\") = ${accept("b")}")
//    println(s"tm_eq_abc.accept(\"bbb\") = ${accept("bbb")}")
//    println(s"tm_eq_abc.accept(\"aabcbcccbbbabc\") = ${accept("aabcbcccbbbabc")}")
//    println(s"tm_eq_abc.accept(\"cb\") = ${accept("cb")}")
//    println(s"tm_eq_abc.accept(\"accbbc\") = ${accept("accbbc")}")
//    println(s"tm_eq_abc.accept(\"abccb\") = ${accept("abccb")}")
//    println(s"tm_eq_abc.accept(\"cabccbcccabbabb\") = ${accept("cabccbcccabbabb")}")
//    println(s"tm_eq_abc.accept(\"bcbabcb\") = ${accept("bcbabcb")}")
//    println(s"tm_eq_abc.accept(\"abcbacbbbccbba\") = ${accept("abcbacbbbccbba")}")


    println("--------------------------------------------------")
  }

  import HeadMove.*

  // TM accpeting L = { a^n b^n c^n | n ≥ 0 }
  val tm_an_bn_cn: TM = TM(
    states = Set(0, 1, 2, 3, 4, 5),
    symbols = Set('a', 'b', 'c'),
    tapeSymbols = Set('a', 'b', 'c', 'X', 'Y', 'Z', 'B'),
    trans = Map(
      (0, 'a') -> (1, 'X', R),
      (0, 'Y') -> (4, 'Y', R),
      (0, 'B') -> (5, 'B', L),
      (1, 'a') -> (1, 'a', R),
      (1, 'Y') -> (1, 'Y', R),
      (1, 'b') -> (2, 'Y', R),
      (2, 'b') -> (2, 'b', R),
      (2, 'Z') -> (2, 'Z', R),
      (2, 'c') -> (3, 'Z', L),
      (3, 'a') -> (3, 'a', L),
      (3, 'b') -> (3, 'b', L),
      (3, 'Y') -> (3, 'Y', L),
      (3, 'Z') -> (3, 'Z', L),
      (3, 'X') -> (0, 'X', R),
      (4, 'Y') -> (4, 'Y', R),
      (4, 'Z') -> (4, 'Z', R),
      (4, 'B') -> (5, 'B', L),
    ),
    initState = 0,
    blank = 'B',
    finalStates = Set(5),
  )

  // TM accepting L = { a^{n^2} | n ≥ 0 }
  def tm_square: TM = TM (
    states = Set(0, 1, 2, 3, 4, 5, 6, 7, 8),
    symbols = Set('a'),
    tapeSymbols = Set('a', 'X', 'Y', 'Z', 'B'),
    trans = Map (
      (0, 'a') -> (1, 'Y', R),
      (0, 'B') -> (7, 'B', R),
      (1, 'a') -> (1, 'a', L),
      (1, 'X') -> (1, 'X', L),
      (1, 'Y') -> (2, 'Y', L),
      (1, 'Z') -> (5, 'Z', R),
      (1, 'B') -> (7, 'B', L),
      (2, 'Y') -> (2, 'Y', L),
      (2, 'Z') -> (3, 'Z', R),
      (2, 'B') -> (3, 'B', R),
      (3, 'Y') -> (4, 'Z', R),
      (4, 'Y') -> (4, 'Y', R),
      (4, 'X') -> (4, 'X', R),
      (4, 'a') -> (1, 'X', R),
      (5, 'X') -> (5, 'Y', R),
      (5, 'a') -> (6, 'Y', R),
      (6, 'a') -> (1, 'Y', R),
      (7, 'Y') -> (7, 'Y', L),
      (7, 'Z') -> (7, 'Z', L),
      (7, 'X') -> (8, 'X', L),
    ),
    initState = 0,
    blank = 'B',
    finalStates = Set(7),
  )

  // TM accepting L = { a^n | n is a fibonacci number }
  def tm_fib: TM = TM (
    states = Set(0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11),
    symbols = Set('a'),
    tapeSymbols = Set('a', 'X', 'Y', 'B'),
    trans = Map(
      (0, 'B') -> (11, 'B', R),
      (0, 'a') -> (1, 'X', R),
      (1, 'B') -> (11, 'B', R),
      (1, 'a') -> (2, 'Y', R),
      (2, 'B') -> (11, 'B', R),
      (2, 'a') -> (3, 'a', L),
      (3, 'Y') -> (4, 'Y', L),
      (3, 'X') -> (7, 'X', L),
      (4, 'Y') -> (4, 'Y', L),
      (4, 'X') -> (5, 'Y', R),
      (4, 'B') -> (10, 'B', R),
      (5, 'X') -> (5, 'X', R),
      (5, 'Y') -> (5, 'Y', R),
      (5, 'a') -> (6, 'X', L),
      (6, 'X') -> (6, 'X', L),
      (6, 'Y') -> (4, 'Y', L),
      (7, 'X') -> (7, 'X', L),
      (7, 'Y') -> (8, 'X', R),
      (7, 'B') -> (10, 'B', R),
      (8, 'X') -> (8, 'X', R),
      (8, 'Y') -> (8, 'Y', R),
      (8, 'a') -> (9, 'Y', L),
      (9, 'Y') -> (9, 'Y', L),
      (9, 'X') -> (7, 'X', L),
      (10, 'X') -> (10, 'X', R),
      (10, 'Y') -> (10, 'Y', R),
      (10, 'a') -> (3, 'a', L),
      (10, 'B') -> (11, 'B', R)
    ),
    initState = 0,
    blank = 'B',
    finalStates = Set(11)
  )

  // TM accepting L = { w \in {a, b, c}* | N_a(w) = N_b(w) = N_c(w) }
  def tm_eq_abc: TM = TM(
    states = Set(0, 1, 2, 3, 4, 5, 6, 7),
    symbols = Set('a', 'b', 'c'),
    tapeSymbols = Set('a', 'b', 'c', 'X', 'B'),
    trans = Map(
      // Search A
      (0, 'X') -> (0, 'X', R),
      (0, 'a') -> (1, 'X', L),
      (0, 'b') -> (0, 'b', R),
      (0, 'c') -> (0, 'c', R),
      (0, 'B') -> (6, 'B', L),
      // Go to start point
      (1, 'X') -> (1, 'X', L),
      (1, 'a') -> (1, 'a', L),
      (1, 'b') -> (1, 'b', L),
      (1, 'c') -> (1, 'c', L),
      (1, 'B') -> (2, 'B', R),
      // Search B
      (2, 'X') -> (2, 'X', R),
      (2, 'a') -> (2, 'a', R),
      (2, 'b') -> (3, 'X', L),
      (2, 'c') -> (2, 'c', R),
      (2, 'B') -> (7, 'B', L),
      // Go to start point
      (3, 'X') -> (3, 'X', L),
      (3, 'a') -> (3, 'a', L),
      (3, 'b') -> (3, 'b', L),
      (3, 'c') -> (3, 'c', L),
      (3, 'B') -> (4, 'B', R),
      // Search C
      (4, 'X') -> (4, 'X', R),
      (4, 'a') -> (4, 'a', R),
      (4, 'b') -> (4, 'b', R),
      (4, 'c') -> (5, 'X', L),
      (4, 'B') -> (7, 'B', L),
      // Go to start point
      (5, 'X') -> (5, 'X', L),
      (5, 'a') -> (5, 'a', L),
      (5, 'b') -> (5, 'b', L),
      (5, 'c') -> (5, 'c', L),
      (5, 'B') -> (0, 'B', R),
      // Check
      (6, 'X') -> (6, 'X', L),
      (6, 'b') -> (7, 'b', L),
      (6, 'c') -> (7, 'c', L)
    ),
    initState = 0,
    blank = 'B',
    finalStates = Set(6)
  )

  // TM for a function f(w ∈ {0, 1}*) = w' where w' = w - 1 if w starts with 1,
  // otherwise f(w) is not defined
  def tm_dec: TM = TM(
    states = Set(-1, 0, 1, 2, 3, 4, 5),
    symbols = Set('0', '1'),
    tapeSymbols = Set('0', '1', 'B'),
    trans = Map(
      (-1, '1') -> (0, '1', R),
      (0, '0') -> (0, '0', R),
      (0, '1') -> (0, '1', R),
      (0, 'B') -> (1, 'B', L),
      (1, '0') -> (1, '1', L),
      (1, '1') -> (2, '0', L),
      (2, '0') -> (2, '0', L),
      (2, '1') -> (2, '1', L),
      (2, 'B') -> (3, 'B', R),
      (3, '0') -> (3, '0', R),
      (3, '1') -> (4, '1', L),
      (3, 'B') -> (5, 'B', L),
      (4, '0') -> (4, 'B', L),
      (4, 'B') -> (5, 'B', R),
      (5, 'B') -> (5, 'B', R),
    ),
    initState = -1,
    blank = 'B',
    finalStates = Set(5)
  )

  // TM for a function f(x+y) = z where x,y ∈ {0, 1}* start with 1 and z = x + y
  def tm_add: TM = TM(
    states = Set(0, 1, 2, 3, 4, 5, 6, 7, 8, -4, -3, -2, -1),
    symbols = Set('0', '1', '+'),
    tapeSymbols = Set('0', '1', '+', 'Z', 'P', 'B'),
    trans = Map(
      (-4, '1') -> (-3, '1', R),
      (-3, '0') -> (-3, '0', R),
      (-3, '1') -> (-3, '1', R),
      (-3, '+') -> (-2, '+', R),
      (-2, '1') -> (0, '1', R),
      (0, '0') -> (0, '0', R),
      (0, '1') -> (0, '1', R),
      (0, '+') -> (0, '+', R),
      (0, 'Z') -> (0, 'Z', R),
      (0, 'P') -> (0, 'P', R),
      (0, 'B') -> (1, 'B', L),

      (1, '0') -> (2, 'B', L),
      (1, '1') -> (4, 'B', L),
      (1, '+') -> (7, 'B', L),

      (2, '0') -> (2, '0', L),
      (2, '1') -> (2, '1', L),
      (2, '+') -> (3, '+', L),

      (3, 'Z') -> (3, 'Z', L),
      (3, 'P') -> (3, 'P', L),
      (3, '0') -> (0, 'Z', R),
      (3, '1') -> (0, 'P', R),
      (3, 'B') -> (0, 'Z', R),

      (4, '0') -> (4, '0', L),
      (4, '1') -> (4, '1', L),
      (4, '+') -> (5, '+', L),

      (5, 'Z') -> (5, 'Z', L),
      (5, 'P') -> (5, 'P', L),
      (5, '0') -> (0, 'P', R),
      (5, '1') -> (6, 'Z', L),
      (5, 'B') -> (0, 'P', R),

      (6, '1') -> (6, '0', L),
      (6, '0') -> (0, '1', R),
      (6, 'B') -> (0, '1', R),

      (7, '0') -> (7, '0', L),
      (7, '1') -> (7, '1', L),
      (7, 'Z') -> (7, '0', L),
      (7, 'P') -> (7, '1', L),
      (7, 'B') -> (8, 'B', R),
    ),
    initState = -4,
    blank = 'B',
    finalStates = Set(8)
  )

}
