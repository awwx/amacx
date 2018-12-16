#lang racket

(require racket/hash)
(require racket/runtime-path)

(require "common.rkt")
(require "runtime.rkt")
(require (only-in (submod "runtime.rkt" mpair)
                  (ar-nillist       mpair:ar-nillist)
                  (eval-ail         mpair:eval-ail)
                  (ref              mpair:ref)
                  (runtime-builtins mpair:builtins)
                  (tnil             mpair:tnil)))

(require (only-in (submod "runtime.rkt" srcloc)
                  (ar-nillist       srcloc:ar-nillist)
                  (eval-ail         srcloc:eval-ail)
                  (ref              srcloc:ref)
                  (runtime-builtins srcloc:builtins)
                  (tnil             srcloc:tnil)))

(require "ail-ns.rkt")

(provide runtimef runtime-builtins)

(define-runtime-path runtime-path "runtime.rkt")

(define (runtimef runtime name)
  (dynamic-require (list 'submod runtime-path runtime) name))

(define (builtin-eval-ail-for runtime)
  (define runtime-eval-ail (runtimef runtime 'eval-ail))
  (define default-namespace (hash-ref default-namespaces runtime))
  (λ (x (ns default-namespace))
    (runtime-eval-ail x ns)))

(define (features runtime container)
  ((runtimef runtime 'ar-nillist) (cons 'scxr (hash-keys container))))

(define (add-features runtime container)
  (hash-set container '*features* (features runtime container)))

(define (complete-builtins runtime)
  (define builtins
    (add-features runtime
      (hash-union (runtimef runtime 'runtime-builtins)
        (hash
          'ail-namespace (λ () (construct-ail-ns runtime))
          'ar-builtins   (λ () builtins)
          'eval-ail      (builtin-eval-ail-for runtime)))))
  builtins)

(define runtime-builtins
  (let ((builtins (map-hash complete-builtins runtimes)))
    (λ (runtime)
      (hash-ref builtins runtime))))
