#lang racket

(provide new-symtab symtab? symtab-has-key? symtab-ref symtab-set! symtab-each)

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

(define (symtab-ref g k)
  (hash-ref (symtab-hash g) k (λ () (error "key not found" k))))

(define (symtab-set! g k v)
  (hash-set! (symtab-hash g) k v))

(define (symtab-each f g)
  (hash-for-each (symtab-hash g) f))
