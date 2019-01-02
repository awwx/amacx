(use arc anarki-test)

; Anarki 407fd9a arc.arc.t:238

(anarki-suite cut
  (anarki-test finds-element-in-list
    (assert-same '(3 4 5) (cut '(1 2 3 4 5) 2)))

  (anarki-test respects-end-index-in-list
    (assert-same '(3 4) (cut '(1 2 3 4 5) 2 4)))

  (anarki-test end-index-at-end-of-list-works
    (assert-same '(3 4 5)
                 (cut '(1 2 3 4 5) 2 5)))

  (anarki-test end-index-beyond-end-of-list-works
    (assert-same '(3 4 5)
                 (cut '(1 2 3 4 5) 2 6)))

  (anarki-test negative-end-index-in-list-is-ok
    (assert-same '(3 4) (cut '(1 2 3 4 5) 2 -1)))

  (anarki-test finds-element-in-string
    (assert-same "cde" (cut "abcde" 2)))

  (anarki-test respects-end-index-in-string
    (assert-same "cd" (cut "abcde" 2 4)))

  (anarki-test end-index-at-end-of-string-works
    (assert-same "cde" (cut "abcde" 2 5)))

  ; Not in Arc 3.2
  ; TODO Anarki extension?
  ; (anarki-test end-index-beyond-end-of-string-works
  ;   (assert-same "cde" (cut "abcde" 2 6)))

  (anarki-test negative-end-index-in-string-is-ok
    (assert-same "cd" (cut "abcde" 2 -1))))
