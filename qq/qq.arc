; Quasiquote ported from GNU clisp 2.47 backquote.lisp to Arc in May
; 2010 by fallintothis
;
; https://web.archive.org/web/20151216173038/https://bitbucket.org/fallintothis/qq/src/04a5dfbc592e5bed58b7a12fbbc34dcd5f5f254f/qq.arc
;
; https://web.archive.org/web/20130812174457/http://arclanguage.org/item?id=9962


; Note that at this point in the load process we don't have
;   quasiquote
;   optional arguments
;   case
;   eval
;   tostring
;   ssyntax such as acons&car or ~caris
;   + on lists or strings

; Like list, except that the last cons of the constructed list is dotted

(def dotted-list xs
  (if (no (cdr xs))
       (car xs)
       (rreduce cons xs)))


(def proper (xs)
  (if (alist xs) (if (no (dotted xs)) t)))


(def qq-non-list-splice-error (expr)
  (err "invalid ,@ syntax"))

(def qq-dotted-splice-error (expr)
  (err "invalid (xs . ,@ys) syntax"))

; Quasiquotation

; Since quasiquote handles 'unquote and 'unquote-splicing, we can define those
; as macros down to errors, as they're automatically outside of a quasiquote.

; (mac unquote (expr)
;   (list 'err "unquote not allowed outside of a quasiquote:" expr))

; (mac unquote-splicing (expr)
;   (list 'err "unquote-splicing not allowed outside of a quasiquote:" expr))


; Recursive Expansion Engine

; The behaviour is more-or-less dictated by the Common Lisp
; HyperSpec's general description of backquote:
;
;   `atom/nil -->  'atom/nil
;   `,expr     -->  expr
;   `,@expr    -->  error
;   ``expr     -->  `expr-expanded
;   `list-expr -->  expand each element & handle dotted tails:
;       `(x1 x2 ... xn)     -->  (append y1 y2 ... yn)
;       `(x1 x2 ... . xn)   -->  (append y1 y2 ... 'xn)
;       `(x1 x2 ... . ,xn)  -->  (append y1 y2 ... xn)
;       `(x1 x2 ... . ,@xn) -->  error
;     where each yi is the output of (qq-transform xi).

(def qq-expand (expr)
  (if (atom expr)
       (list quote expr)
       (let x (car expr)
         (if (is x 'unquote)
              (cadr expr)
             (is x 'unquote-splicing)
              (qq-non-list-splice-error (cadr expr))
             (is x 'quasiquote)
              (list 'quasiquote (qq-expand (cadr expr)))
              (qq-appends (qq-expand-list expr))))))

; Produce a list of forms suitable for append.
; Note: if we see 'unquote or 'unquote-splicing in the middle of a list, we
; assume it's from dotting, since (a . (unquote b)) == (a unquote b).
; This is a "problem" if the user does something like `(a unquote b c d), which
; we interpret as `(a . ,b).

(def qq-expand-list (expr)
  (and expr
       (if (atom expr)
            (list (list quote expr))
            (let x (car expr)
              (if (is x 'unquote)
                   (list (cadr expr))
                  (is x 'unquote-splicing)
                   (qq-dotted-splice-error (cadr expr))
                   (cons (qq-transform (car expr))
                         (qq-expand-list (cdr expr))))))))

; Do the transformations for elements in qq-expand-list that aren't the dotted
; tail.  Also, handle nested quasiquotes.

(def qq-transform (expr)
  (let x (and (acons expr) (car expr))
    (if (is x 'unquote)
         (qq-list (cadr expr))
        (is x 'unquote-splicing)
         (cadr expr)
        (is x 'quasiquote)
         (qq-list (list 'quasiquote (qq-expand (cadr expr))))
         (qq-list (qq-expand expr)))))


; Expansion Optimizer

; This is mainly woven through qq-cons and qq-append.  It can run in a
; non-optimized mode (where lists are always consed at run-time), or
; optimizations can be done that reduce run-time consing / simplify the
; macroexpansion.  For example,
;   `(,(foo) ,(bar))
;      non-optimized --> (append (cons (foo) nil) (cons (bar) nil))
;      optimized     --> (list (foo) (bar))

; Optimization is enabled by default, but can be turned off for debugging.

(assign qq-optimize-cons* t)
(assign qq-optimize-append* t)

; Test whether the given expr may yield multiple list elements.
; Note: not only does ,@x splice, but so does ,,@x (unlike in vanilla Arc)

(def qq-splicing (expr)
  (let x (and (acons expr) (car expr))
    (if (is x 'unquote-splicing)
         t
        (is x 'unquote)
         (qq-splicing (cadr expr)))))

(def splicing->non (expr)
  (if (qq-splicing expr) (list 'join expr) expr))

(def quoted-non-splice (expr)
  (and (caris expr 'quote)
        (single (cdr expr))
        (no (qq-splicing (cadr expr)))))

; Assumes expr2 is non-splicing

(def qq-cons (expr1 expr2)
  (let operator (if (qq-splicing expr1) 'dotted-list 'cons)
    (if (no qq-optimize-cons*)
         (list operator expr1 expr2)
        (and (no (qq-splicing expr1)) (literal expr1) (no expr2))
         (list 'quote (list (literal-value expr1)))
        (no expr2)
          (list 'list expr1)
        (atom expr2)
         (list operator expr1 expr2)
        (caris expr2 'list)
         (dotted-list 'list expr1 (cdr expr2))
        (and (quoted-non-splice expr1) (quoted-non-splice expr2))
         (list 'quote (cons (cadr expr1) (cadr expr2)))
         (list operator expr1 expr2))))

(def qq-list (expr)
  (qq-cons expr nil))

; (def qq-append (expr1 (o expr2)) ...)

(def qq-append (expr1 . rest)
  (let expr2 (if (acons rest) (car rest))
    (if (no qq-optimize-append*)
         (list 'join expr1 expr2)
        (no expr1)
         expr2
        (no expr2)
         expr1
        (caris expr1 'list)
         (if (single expr1)
              expr2
             (single (cdr expr1))
              (qq-cons (cadr expr1) expr2)
              (cons 'dotted-list (join (cdr expr1) (list expr2))))
        (and (quoted-non-splice expr1)
             (proper (cadr expr1))
             ; since unquote expects only 1 arg
             (no (caris (cadr expr1) 'unquote)))
         (rreduce (fn (x xs) (qq-cons (list 'quote x) xs))
                  (join (cadr expr1) (list (splicing->non expr2))))
        (caris expr2 'join)
         (dotted-list 'join expr1 (cdr expr2))
         (list 'join expr1 expr2))))

(def qq-appends (exprs) (splicing->non (rreduce qq-append exprs)))

(mac quasiquote (expr)
  (qq-expand expr))
