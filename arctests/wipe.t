(use arc equals iso-table)

(with (a 3 b 4 c 5)
  (wipe a)
  (equals a nil)
  (wipe b c)
  (equals b nil)
  (equals c nil))

(let h (obj a 1 b 2 c 3)
  (wipe h!b h!a)
  (equals h (obj c 3)))
