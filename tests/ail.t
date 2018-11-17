((*module* ($quote ar-assert))
 ((*module* ($quote ar-is2))
  ($if ($quote t) ($quote 118) ($quote wrong))
  ($quote 118)))

((*module* ($quote ar-assert))
 ((*module* ($quote ar-is2))
  ($if ($quote nil) ($quote wrong) ($quote 119))
  ($quote 119)))

((*module* ($quote ar-assert))
 ((*module* ($quote ar-is2))
  (($fn () ($quote 10)))
  ($quote 10)))

((*module* ($quote ar-assert))
 ((*module* ($quote ar-is2))
  (($fn (a) a) ($quote 11))
  ($quote 11)))

((*module* ($quote ar-assert))
 ((*module* ($quote ar-is2))
  (($fn (a b) a) ($quote 20) ($quote 21))
  ($quote 20)))

((*module* ($quote ar-assert))
 ((*module* ($quote ar-is2))
  (($fn (a b) b) ($quote 30) ($quote 31))
  ($quote 31)))

((*module* ($quote ar-assert))
 ((*module* ($quote ar-is2))
  (($fn args args))
  ($quote nil)))
