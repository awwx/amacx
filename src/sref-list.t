(use arcbase equals mlist)

(let xs (mcopy '(a b c d))
  (sref xs 0 'X)
  (equals xs '(X b c d))
  (sref xs 1 'Y)
  (equals xs '(X Y c d))
  (sref xs 2 'Z)
  (equals xs '(X Y Z d)))
