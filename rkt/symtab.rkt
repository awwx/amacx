#lang racket

(provide new-symtab symtab? symtab-has-key? symtab-ref symtab-ref-default
         symtab-set! symtab-each symtab-rm)

(struct symtab (hash) #:transparent)

(define new-symtab
  (case-lambda
    (()
      (symtab (make-hash)))

    ((x)
      (cond ((eq? x 'nil)
             (symtab (make-hash)))
            ((hash? x)
             (symtab (hash-copy x)))
            ((error "invalid argument to symtab" x))))))

(define (symtab-has-key? g k)
  (hash-has-key? (symtab-hash g) k))

(define (symtab-ref-default g k default)
  (hash-ref (symtab-hash g) k default))

(define (symtab-ref g k)
  (symtab-ref-default g k (λ () (error "key not found" k))))

(define (symtab-set! g k v)
  (hash-set! (symtab-hash g) k v))

(define (symtab-each g f)
  (hash-for-each (symtab-hash g) f))

(define (symtab-rm g k)
  (hash-remove! (symtab-hash g) k))
