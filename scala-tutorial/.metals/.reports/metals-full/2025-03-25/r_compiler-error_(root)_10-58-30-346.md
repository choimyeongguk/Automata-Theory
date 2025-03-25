error id: 9880B6435FB1BC457DE794FC2B8871E5
file:///C:/Users/CHLAR/Desktop/Theory%20of%20Computation/scala-tutorial/src/main/scala/kuplrg/Implementation.scala
### java.nio.file.InvalidPathException: Illegal char <:> at index 3: jar:file:///C:/Users/CHLAR/AppData/Local/Coursier/cache/v1/https/repo1.maven.org/maven2/org/scala-lang/scala-library/2.13.12/scala-library-2.13.12-sources.jar!/scala/collection/immutable/List.scala

occurred in the presentation compiler.



action parameters:
offset: 1356
uri: file:///C:/Users/CHLAR/Desktop/Theory%20of%20Computation/scala-tutorial/src/main/scala/kuplrg/Implementation.scala
text:
```scala
package kuplrg

import scala.compiletime.ops.boolean

object Implementation extends Template {

  // ---------------------------------------------------------------------------
  // Basic Data Types
  // ---------------------------------------------------------------------------
  def isEvenPair(x: Int, y: Int): Boolean = {
    ((x + y) % 2 == 0)
  }

  def validString(str: String, lower: Int, upper: Int): Boolean = {
    (lower <= str.length && str.length <= upper)
  }

  // ---------------------------------------------------------------------------
  // Functions
  // ---------------------------------------------------------------------------
  def factorial(n: Int): Int = {
    if (n == 1) 1
    else n * factorial(n - 1)
  }

  def magic(x: Int): Int => Int = {   // 함수를 반환하는 함수
    (y: Int) => {
      if (x == 0) (x + 1) * y + (x - y % x)
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

  def merge(l: List[Int]): List[Int] = l matc@@{

  }

  def generate(init: Int, f: Int => Int, n: Int): List[Int] = ???

  def incKey(map: Map[String, Int], key: String): Map[String, Int] = ???

  def validSums(
    l: List[Int],
    r: List[Int],
    f: (Int, Int) => Boolean,
  ): Set[Int] = ???

  // ---------------------------------------------------------------------------
  // Trees
  // ---------------------------------------------------------------------------
  import Tree.*

  def count(t: Tree, x: Int): Int = ???

  def heightOf(t: Tree): Int = ???

  def min(t: Tree): Int = ???

  def sumLeaves(t: Tree): Int = ???

  def inorder(t: Tree): List[Int] = ???

  // ---------------------------------------------------------------------------
  // Boolean Expressions
  // ---------------------------------------------------------------------------
  import BE.*

  def isLiteral(expr: BE): Boolean = ???

  def countImply(expr: BE): Int = ???

  def literals(expr: BE): List[Boolean] = ???

  def getString(expr: BE): String = ???

  def eval(expr: BE): Boolean = ???
}

```


presentation compiler configuration:
Scala version: 3.3.3
Classpath:
<WORKSPACE>\.bloop\root\bloop-bsp-clients-classes\classes-Metals-sSbPqpD1STSC_wskqTJ44Q== [exists ], <HOME>\AppData\Local\Coursier\cache\v1\https\repo1.maven.org\maven2\com\sourcegraph\semanticdb-javac\0.10.3\semanticdb-javac-0.10.3.jar [exists ], <WORKSPACE>\lib\warts.jar [exists ], <HOME>\AppData\Local\Coursier\cache\v1\https\repo1.maven.org\maven2\org\scala-lang\scala3-library_3\3.3.3\scala3-library_3-3.3.3.jar [exists ], <HOME>\AppData\Local\Coursier\cache\v1\https\repo1.maven.org\maven2\org\scala-lang\scala-library\2.13.12\scala-library-2.13.12.jar [exists ]
Options:
-feature -deprecation -explain -explain-types -language:implicitConversions -Xsemanticdb -sourceroot <WORKSPACE>




#### Error stacktrace:

```
java.base/sun.nio.fs.WindowsPathParser.normalize(WindowsPathParser.java:204)
	java.base/sun.nio.fs.WindowsPathParser.parse(WindowsPathParser.java:175)
	java.base/sun.nio.fs.WindowsPathParser.parse(WindowsPathParser.java:77)
	java.base/sun.nio.fs.WindowsPath.parse(WindowsPath.java:92)
	java.base/sun.nio.fs.WindowsFileSystem.getPath(WindowsFileSystem.java:203)
	java.base/java.nio.file.Path.of(Path.java:148)
	java.base/java.nio.file.Paths.get(Paths.java:69)
	scala.meta.io.AbsolutePath$.apply(AbsolutePath.scala:58)
	scala.meta.internal.metals.MetalsSymbolSearch.$anonfun$definitionSourceToplevels$2(MetalsSymbolSearch.scala:70)
	scala.Option.map(Option.scala:242)
	scala.meta.internal.metals.MetalsSymbolSearch.definitionSourceToplevels(MetalsSymbolSearch.scala:69)
	scala.meta.internal.pc.completions.CaseKeywordCompletion$.scala$meta$internal$pc$completions$CaseKeywordCompletion$$$sortSubclasses(MatchCaseCompletions.scala:331)
	scala.meta.internal.pc.completions.CaseKeywordCompletion$.matchContribute(MatchCaseCompletions.scala:279)
	scala.meta.internal.pc.completions.Completions.advancedCompletions(Completions.scala:393)
	scala.meta.internal.pc.completions.Completions.completions(Completions.scala:186)
	scala.meta.internal.pc.completions.CompletionProvider.completions(CompletionProvider.scala:91)
	scala.meta.internal.pc.ScalaPresentationCompiler.complete$$anonfun$1(ScalaPresentationCompiler.scala:147)
```
#### Short summary: 

java.nio.file.InvalidPathException: Illegal char <:> at index 3: jar:file:///C:/Users/CHLAR/AppData/Local/Coursier/cache/v1/https/repo1.maven.org/maven2/org/scala-lang/scala-library/2.13.12/scala-library-2.13.12-sources.jar!/scala/collection/immutable/List.scala