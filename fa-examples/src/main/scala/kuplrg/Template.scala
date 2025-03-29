package kuplrg

trait Template {
  def dfa_a_b_plus: DFA
  def dfa_div_3_2: DFA
  def dfa_subseq_011: DFA
  def dfa_even_0_1: DFA
  def nfa_most_three_0: NFA
  def nfa_not_substr_bab: NFA
  def nfa_comb_aaa_bb: NFA
  def enfa_aba_plus: ENFA
  def enfa_same_digits: ENFA
  def enfa_complex: ENFA
}
