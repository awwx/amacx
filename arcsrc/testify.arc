(use arcbase a-fn square-fn)

(def testify (x)
  (if (a-fn x) x [is _ x]))
