package kuplrg

import scala.compiletime.ops.boolean

object Implementation extends Template {

  // ---------------------------------------------------------------------------
  // Basic Data Types
  // ---------------------------------------------------------------------------
  def isEvenPair(x: Int, y: Int): Boolean = {
    (x + y) % 2 == 0  // 굳이 return 쓰지 않아도 반환됨
  }

  def validString(str: String, lower: Int, upper: Int): Boolean = {
    lower <= str.length && str.length <= upper  // 문자열.length 로 길이 반환
  }

  // ---------------------------------------------------------------------------
  // Functions
  // ---------------------------------------------------------------------------
  def factorial(n: Int): Int = n match {  // C의 switch case 문 비슷한 거
    case 0 => 1                           // 0일 때
    case _ => n * factorial(n - 1)        // otherwise
  }

  def magic(x: Int): Int => Int = { // 함수를 반환하는 함수
    (y: Int) =>
      {
        if (y % x != 0) (x + 1) * y + (x - y % x)
        else y / x
      }
  }

  def applyK(f: Int => Int, k: Int): Int => Int = {
    if (k == 0) (x: Int) => x
    else (x: Int) => applyK(f, k - 1)(f(x))
  }

  // ---------------------------------------------------------------------------
  // Collections
  // ---------------------------------------------------------------------------
  def productPos(l: List[Int]): Int = {
    l.filter(_ > 0).product
  }

  def merge(l: List[Int]): List[Int] = l match {
    case Nil              => Nil
    case x :: Nil         => x :: Nil
    case x :: y :: remain => x + y :: merge(remain)   // :: 연산자는 '값::리스트' 형태를 리스트로 만들어줌
  }

  def generate(init: Int, f: Int => Int, n: Int): List[Int] = n match {
    case 0 => Nil
    case _ => init :: generate(f(init), f, n - 1)
  }

  def incKey(map: Map[String, Int], key: String): Map[String, Int] = {
    if (map.contains(key)) map.updated(key, map(key) + 1)
    else map
  }

  def validSums(
    l: List[Int],
    r: List[Int],
    f: (Int, Int) => Boolean,
  ): Set[Int] = {
    (for {
      x <- l              // x, y에 대해 이중 반복문
      y <- r
      if f(x, y)
    } yield x + y).toSet  // 조건에 맞춰 리스트로 만든 후 중복 제거 
//    l.flatMap(x => r.filter(y => f(x, y)).map(y => x + y)).toSet
  }

  // ---------------------------------------------------------------------------
  // Trees
  // ---------------------------------------------------------------------------
  import Tree.*

  def count(t: Tree, x: Int): Int = t match {
    case Leaf(n) if n == x         => 1
    case Leaf(_)                   => 0
    case Branch(l, n, r) if n == x => count(l, x) + count(r, x) + 1
    case Branch(l, _, r)           => count(l, x) + count(r, x)
  }

  def heightOf(t: Tree): Int = t match {
    case Leaf(_) => 0
//    case Branch(l, _, r) => {
//      val lh: Int = heightOf(l)
//      val rh: Int = heightOf(r)
//      if (lh > rh) lh + 1
//      else rh + 1
//    }
    case Branch(l, _, r) => 1 + math.max(heightOf(l), heightOf(r))  // 더 함수형스럽다
  }

  def min(t: Tree): Int = t match {
    case Leaf(n) => n
//    case Branch(l, n, r) => {
//      val lmin: Int = min(l)
//      val rmin: Int = min(r)
//      if (n < lmin && n < rmin) n
//      else if (lmin < rmin) lmin
//      else rmin
//    }
    case Branch(l, n, r) => math.min(n, math.min(min(l), min(r)))   // 더 간결하다
  }

  def sumLeaves(t: Tree): Int = t match {
    case Leaf(n)         => n
    case Branch(l, _, r) => sumLeaves(l) + sumLeaves(r)
  }

  def inorder(t: Tree): List[Int] = t match {
    case Leaf(v)         => List(v)
    case Branch(l, v, r) => inorder(l) ++ List(v) ++ inorder(r)
  }

  // ---------------------------------------------------------------------------
  // Boolean Expressions
  // ---------------------------------------------------------------------------
  import BE.*

  def isLiteral(expr: BE): Boolean = expr match {
    case Literal(_) => true
    case _          => false
  }

  def countImply(expr: BE): Int = expr match {
    case Literal(_)  => 0
    case And(l, r)   => countImply(l) + countImply(r)
    case Or(l, r)    => countImply(l) + countImply(r)
    case Imply(l, r) => countImply(l) + countImply(r) + 1
    case Not(e)      => countImply(e)
  }

  def literals(expr: BE): List[Boolean] = expr match {
    case Literal(v)  => List(v)
    case And(l, r)   => literals(l) ++ literals(r)
    case Or(l, r)    => literals(l) ++ literals(r)
    case Imply(l, r) => literals(l) ++ literals(r)
    case Not(v)      => literals(v)
  }

  def getString(expr: BE): String = expr match {
    case Literal(v) => {
      if (v == true) "#t"
      else "#f"
    }
    case And(l, r)   => "(" + getString(l) + " & " + getString(r) + ")"
    case Or(l, r)    => "(" + getString(l) + " | " + getString(r) + ")"
    case Imply(l, r) => "(" + getString(l) + " => " + getString(r) + ")"
    case Not(v)      => "!" + getString(v)
  }

  def eval(expr: BE): Boolean = expr match {
    case Literal(v)  => v
    case And(l, r)   => eval(l) && eval(r)
    case Or(l, r)    => eval(l) || eval(r)
    case Imply(l, r) => !eval(l) || eval(r)
    case Not(v)      => !eval(v)
  }
}
