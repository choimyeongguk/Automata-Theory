package kuplrg

import Implementation.*

class Spec extends SpecBase {

  // Number of trials for `mustEqual`
  private val TRIAL = 20_000

  // L = { a b^n | n >= 1 }
  val lang_a_b_plus: Lang = Lang(
    "ab".toSet,
    "ab+".r.matches,
  )

  // tests for `dfa_a_b_plusj`
  check(dfa_a_b_plus.mustValid.mustEqual(lang_a_b_plus, TRIAL))

  // L = { w \in {0, 1}* | N(w) \equiv 2 (mod 3) }
  // where N(w) is the natural number represented by w in binary
  val lang_div_3_2: Lang = Lang(
    "01".toSet,
    w => toInt(w) % 3 == 2,
  )

  // tests for `dfa_div_3_2`
  check(dfa_div_3_2.mustValid.mustEqual(lang_div_3_2, TRIAL))

  // L = { w \in {0, 1}* | w contains a subsequence 011 }
  val lang_subseq_011: Lang = Lang(
    "01".toSet,
    ".*0.*1.*1.*".r.matches,
  )

  // tests for `dfa_subseq_011`
  check(dfa_subseq_011.mustValid.mustEqual(lang_subseq_011, TRIAL))

  // L = { w \in {0, 1}* | w contains even number of 0's and even number of 1's }
  val lang_even_0_1: Lang = Lang(
    "01".toSet,
    w => w.count(_ == '0') % 2 == 0 && w.count(_ == '1') % 2 == 0,
  )

  // tests for `dfa_even_0_1`
  check(dfa_even_0_1.mustValid.mustEqual(lang_even_0_1, TRIAL))

  // L = { w \in {0, 1}* | w contains at most three 0's }
  val lang_most_three_0: Lang = Lang(
    "01".toSet,
    _.count(_ == '0') <= 3,
  )

  // tests for `nfa_most_three_0`
  check(nfa_most_three_0.mustValid.mustEqual(lang_most_three_0, TRIAL))

  // L = { (ab)^n | n >= 0 }
  val lang_not_substr_bab: Lang = Lang(
    "ab".toSet,
    !_.contains("bab"),
  )

  // tests for `nfa_not_substr_bab`
  check(nfa_not_substr_bab.mustValid.mustEqual(lang_not_substr_bab, TRIAL))

  // L = { w \in {a, b}* | w is any combination of "aaa" and "bb" }
  val lang_comb_aaa_bb: Lang = Lang(
    "ab".toSet,
    "(aaa|bb)*".r.matches,
  )

  // tests for `nfa_comb_aaa_bb`
  check(nfa_comb_aaa_bb.mustValid.mustEqual(lang_comb_aaa_bb, TRIAL))

  // L = { (aba)^n | n >= 1 }
  val lang_aba_plus: Lang = Lang(
    "ab".toSet,
    "(aba)+".r.matches,
  )

  // tests for `enfa_aba_plus`
  check(enfa_aba_plus.mustValid.mustEqual(lang_aba_plus, TRIAL))

  // L = { 0^n | n >= 1 } U { 1^n | n >= 1 }
  val lang_same_digits: Lang = Lang(
    "01".toSet,
    w => w != "" && (w.forall(_ == '0') || w.forall(_ == '1')),
  )

  // tests for `enfa_same_digits`
  check(enfa_same_digits.mustValid.mustEqual(lang_same_digits, TRIAL))

  // L = L_1 \cap L_2
  // where
  // L_1 = { w \in {0, 1} | N(w) \equiv 1 (mod 3) }
  // L_2 = { w \in {0, 1} | zeros(w) \equiv 1 (mod 3) }
  // N(w) is the natural number represented by w in binary,
  // and zeros(w) is the number of 0's in w
  val lang_complex: Lang = Lang(
    "01".toSet,
    w => toInt(w) % 3 == 1 && w.count(_ == '0') % 3 == 1,
  )

  // tests for `enfa_complex`
  check(enfa_complex.mustValid.mustEqual(lang_complex, TRIAL))
}
