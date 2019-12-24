# Correctness of a Simple Compiler for Expressions in Reverse-Polish Notation

A proof for a simple compiler for expressions in reverse-polish notation. Based on Dr. William Young's original proof, this proof guides <a href="http://www.cs.utexas.edu/users/moore/acl2/">ACL2</a>, an automated theorem prover, to prove the correctness of a compiler for reverse-polish notation expressions with valid operations +,-, and *.

The approach: 

<ol>
  <li> Define valid polish-notation expressions and how to evaluate these expressions in lisp </li>
  <li> Define how the compiler converts these expressions to PUSH, ADD, SUB, and MULT operations  </li>
  <li> Describe how the compiler evaluates these compiled expressions </li>
  <li> Prove that the evaluation of the original expression and the compiled expression are equal </li>
  
</ol>
