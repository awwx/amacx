(use arcbase square-fn equals andf even)

(let plus1 [+ _ 1]
  (equals ((andf even plus1) 1) nil)
  (equals ((andf even plus1) 2) 3)
  (equals ((andf even plus1) 3) nil))
