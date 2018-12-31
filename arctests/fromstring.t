(use arcbase equals accum fromstring readc)

(equals
  (accum a
    (fromstring "abc"
      (a (readc))
      (a (readc))
      (a (readc))
      (a (readc))))
  '(#\a #\b #\c nil))
