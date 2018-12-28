(use arcbase quasiquote after outfile)

(let expander
     (fn (f var name body)
       `(,let ,var (,f ,name)
          (,after (,do ,@body) (,close ,var))))

  (mac w/infile (var name . body)
    (expander infile var name body))

  (mac w/outfile (var name . body)
    (expander outfile var name body))

  (mac w/instring (var str . body)
    (expander instring var str body))

  (mac w/socket (var port . body)
    (expander open-socket var port body)))
