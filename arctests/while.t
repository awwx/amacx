(use equals with while isnt assign)

(equals (with (i 5 x 0)
          (while (isnt (assign i (- i 1)) 0)
            (assign x (+ x i)))
          x)
        10)
