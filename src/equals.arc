(use arcbase w/uniq quasiquote catcherr iso disp write)

(provides simple-equals)

(mac equals (x expected)
  (w/uniq (actual expect)
    `(with (,actual (catcherr ,x) ,expect ,expected)

       (if (iso ,actual ,expect)

            (do (disp "OK ")
                (write ',x)
                (disp " => ")
                (write ,actual)
                (disp "\n"))

            (do (disp "FAIL ")
                (write ',x)
                (disp " => ")
                (disp "\n")
                (write ,actual)
                (disp " NOT")
                (disp "\n")
                (write ,expect)
                (disp "\n")
                (quit 1))))))
