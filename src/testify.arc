(use simple-def if a-fn is square-fn)

(def testify (x)
  (if (a-fn x) x [is _ x]))
