#lang racket

(require racket/runtime-path)

(provide rootdir map-hash nth-set! all-tests)

(define-runtime-path here "here")

(define rootdir (path->string (simplify-path (build-path here 'up 'up))))

(define (map-hash f keys)
  (if (null? keys)
      (hash)
      (hash-set (map-hash f (cdr keys))
                (car keys)
                (f (car keys)))))

(define (mlist-tail lst n)
  (if (= n 0)
       lst
       (mlist-tail (mcdr lst) (- n 1))))

(define (nth-set! lst n val)
  (set-mcar! (mlist-tail lst n) val))

(define srcdirs '("arcsrc" "arctests" "qq" "qqtests" "src" "xboot"))

(define (tests-in-dir dir)
  (map (λ (filename)
         (path->string (build-path dir filename)))
       (filter (λ (filename)
                 (string-suffix? (path->string filename) ".t"))
               (directory-list (build-path rootdir dir)))))

(define (all-tests)
  (append-map tests-in-dir srcdirs))
