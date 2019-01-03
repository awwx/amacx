(use arc equals)

(let g (obj)
  (pushnew 'a g!x)
  (equals g!x '(a))

  (pushnew 'a g!x)
  (equals g!x '(a))

  (pushnew 'b g!x)
  (equals g!x '(b a)))
