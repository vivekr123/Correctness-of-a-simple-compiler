;; Defining an expression language w/ the operators +,  -, *

; back quote (`) creates a list like ' would, however it evaluates expressions after , (therefore x will be evaluated in the following code)
; first, second, and third are macros that extract the appropriate element
(defmacro op (x) `(first ,x))
(defmacro arg1 (x) `(second ,x))
(defmacro arg2 (x) `(third ,x))

;; An expression in this language is a list of tokens, each of which
;; is an integer or +.   I'll add others later

; added -, *
(defun oprp (x)
  (member-equal x '(+ - *))) ;; we now add -, *

; no change
(defun exprp (l)
  (if (endp l)
      (equal l nil)
    (or (and (natp (car l))
         (exprp (cdr l)))
    (and (oprp (car l))
         (exprp (cdr l))))))
           

;; This is the evaluator.  We're not sure that there are 
;; enough args on the stack at this point.

; added -, * cases
(defun eval-expr (x stack)
  (if (endp x)
      stack
    (if (natp (car x))
    (eval-expr (cdr x) (cons (car x) stack))
      (let* ((a (car stack))
         (b (cadr stack))
         (new-stack (cons (+ b a) (cddr stack)))
         (new-stack2 (cons (- b a) (cddr stack)))
         (new-stack3 (cons (* b a) (cddr stack))))
    (case (op x)
          (+ (eval-expr (cdr x) new-stack))
          (- (eval-expr (cdr x) new-stack2))
          (* (eval-expr (cdr x) new-stack3)))))))

; ------------------------------------------------------------------------------
; Define a low level stack-based langauge w/
; PUSH, ADD, MULT, SUB -> the "instructions"

; simple function just for seeing if list is of a certain size
(defun ex-lenp (x n)
  (and (true-listp x)
       (equal (len x) n)))

; is this a push instruction?
(defun push-instp (x)
  (and (ex-lenp x 2)
       (equal (op x) 'PUSH)
       (natp (arg1 x))))

; added SUB, MULT
; is this an instruction?
(defun instp (x)
  (or (push-instp x)
      ; must be an instruction in this list 
      (member-equal x '(ADD SUB MULT))))

; valid list of instructions?
(defun inst-listp (x)
  (if (endp x)
    t
    (and (instp (first x))
         (inst-listp (rest x)))))

; added -, * in case
(defun compile-expr (expr)
  (if (endp expr)
      nil
    (let ((x (car expr))
      (r (cdr expr)))
      (if (natp x)
      (cons (list 'PUSH x)
        (compile-expr r))
      (case (op expr)
            (+ (cons 'ADD (compile-expr r)))
            (- (cons 'SUB (compile-expr r)))
            (otherwise (cons 'MULT (compile-expr r))))))))

(defun run-program (prog stack)
  (if (endp prog)
      stack
    (if (push-instp (car prog))
    (run-program (cdr prog)
             (cons (cadar prog) stack))
    (case (car prog)
          ('ADD (run-program (cdr prog) (cons (+ (cadr stack) (car stack)) (cddr stack))))
          ('SUB (run-program (cdr prog) (cons (- (cadr stack) (car stack)) (cddr stack))))
          ('MULT (run-program (cdr prog) (cons (* (cadr stack) (car stack)) (cddr stack))))
      ))))

(defun stack-depth-ok (expr k)
  ;; This checks that the stack depth would always be OK
  ;; during the evaluation of expr, assuming that it starts
  ;; at k.
  (if (endp expr)
      (<= 0 k)
    (let ((a (car expr)))
      (if (natp a)
      (stack-depth-ok (cdr expr) (1+ k))
    (and (<= 2 k)
         (stack-depth-ok (cdr expr) (1- k)))))))

; Not needed
#|
(defthm stack-nat-listp
  ;; The stack is always going to be a nat-listp
  (implies (and (nat-listp stack)
        (exprp exp))
       (nat-listp (eval-expr exp stack))))

|#

(defthm compile-inst-listp
  ;; Compiling a legal expression gives an inst-listp
  (implies (exprp exp)
       (inst-listp (compile-expr exp))))

(set-irrelevant-formals-ok t)#|ACL2s-ToDo-Line|#


;; induction hint
(defun eval-compile-induction (expr stack)
  (if (endp expr)
      t
    (if (natp (car expr))
    (eval-compile-induction (cdr expr)
                (cons (car expr) stack))
      (eval-compile-induction (cdr expr)
                  (cons (+ (car stack) (cadr stack))
                    (cddr stack))))))

;; final theorem to prove (correctness of compiler)
(defthm eval-compile
  (implies (and (exprp expr)
        (stack-depth-ok expr (len stack)))
       (equal (eval-expr expr stack)
          (run-program (compile-expr expr)
                   stack))))
;;  induction hint not needed
;;  :hints (("goal" :induct (eval-compile-induction expr stack))))
