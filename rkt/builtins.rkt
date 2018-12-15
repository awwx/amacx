#lang racket

(require racket/hash)
(require racket/runtime-path)

(require (only-in (submod "runtime.rkt" mpair)
                  (ar-nillist       mpair:ar-nillist)
                  (eval-ail         mpair:eval-ail)
                  (quote-this       mpair:quote-this)
                  (ref              mpair:ref)
                  (runtime-builtins mpair:builtins)
                  (tnil             mpair:tnil)))

(require (only-in (submod "runtime.rkt" srcloc)
                  (ar-nillist       srcloc:ar-nillist)
                  (eval-ail         srcloc:eval-ail)
                  (quote-this       srcloc:quote-this)
                  (ref              srcloc:ref)
                  (runtime-builtins srcloc:builtins)
                  (tnil             srcloc:tnil)))

(require "ail-ns.rkt")

(provide runtimef xruntime)

(define-runtime-path runtime-path "runtime.rkt")

(define (runtimef runtime name)
  (dynamic-require (list 'submod runtime-path runtime) name))

(define (mpair:builtin-eval-ail x (ns default-mpair-namespace))
  (mpair:eval-ail x ns))

(define (srcloc:builtin-eval-ail x (ns default-srcloc-namespace))
  (srcloc:eval-ail x ns))

(define (features runtime container)
  ((runtimef runtime 'ar-nillist) (cons 'scxr (hash-keys container))))

(define (add-features runtime container)
  (hash-set container '*features* (features runtime container)))

(define mpair-builtins
  (add-features 'mpair
    (hash-union (runtimef 'mpair 'runtime-builtins)
      (hash
        'ail-namespace (λ () (ail-ns 'mpair))
        'ar-builtins   (λ () mpair-builtins)
        'eval-ail      mpair:builtin-eval-ail))))

(define srcloc-builtins
  (add-features 'srcloc
    (hash-union (runtimef 'srcloc 'runtime-builtins)
      (hash
        'ail-namespace (λ () (ail-ns 'srcloc))
        'ar-builtins   (λ () srcloc-builtins)
        'eval-ail      srcloc:builtin-eval-ail))))

(define mpair-runtime-hash
  (hash 'ar-nillist mpair:ar-nillist
        'builtins   mpair-builtins
        'namespace  default-mpair-namespace
        'quote-this mpair:quote-this
        'ref        mpair:ref
        'tnil       mpair:tnil))

(define (mpair-runtime x)
  (hash-ref mpair-runtime-hash x))

(define srcloc-runtime-hash
  (hash 'ar-nillist  srcloc:ar-nillist
        'builtins    srcloc-builtins
        'namespace   default-srcloc-namespace
        'quote-this  srcloc:quote-this
        'ref         srcloc:ref
        'tnil        srcloc:tnil))

(define (srcloc-runtime x)
  (hash-ref srcloc-runtime-hash x))

(define (xruntime n)
  (case n
    ((mpair)  mpair-runtime)
    ((srcloc) srcloc-runtime)
    (else (error "unknown runtime" n))))
