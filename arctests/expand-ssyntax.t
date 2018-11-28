(use equals expand-ssyntax square-fn)

(equals (ssyntax-char #\A) nil)
(equals (ssyntax-char #\:) t)

(equals (has-ssyntax-char "abc")   nil)
(equals (has-ssyntax-char "ab:cd") t)

(equals (is-ssyntax 'abc)   nil)
(equals (is-ssyntax '++)    nil)
(equals (is-ssyntax ':)     t)
(equals (is-ssyntax 'ac.cd) t)
(equals (is-ssyntax 'ab:cd) t)

(equals (insym #\: 'a:b) t)
(equals (insym #\: 'abc) nil)

(equals (ssyntax-tokens #\: '(#\a #\b #\c #\: #\d #\e #\f) nil nil nil)
  '((#\a #\b #\c) (#\d #\e #\f)))

(let check
     (fn (a b)
       (equals (ssyntax-tokens [in _ #\. #\!]
                               (strchars (as-str a))
                               nil nil t)
               b))
  (check 'a.b!c  '((#\a) #\. (#\b) #\! (#\c)))
  (check 'a!b!!  '((#\a) #\! (#\b) #\! #\!)))

(equals (chars->value '(#\a \b)) 'ab)
(equals (chars->value '(#\1 \2)) 12)

(equals (symbol->chars 'xy) '(#\x #\y))

(equals (ssyntax-expand-compose 'a:b)   '(compose a b))
(equals (ssyntax-expand-compose 'a:b:c) '(compose a b c))
(equals (ssyntax-expand-compose '~)     'no)
(equals (ssyntax-expand-compose '~a)    '(complement a))
(equals (ssyntax-expand-compose '~a:b)  '(compose (complement a) b))
(equals (ssyntax-expand-compose 'a:~b)  '(compose a (complement b)))
(equals (ssyntax-expand-compose '~a:~b) '(compose (complement a) (complement b)))

(equals (ssyntax-build-sexpr '((#\b) #\! (#\a)) 'a!b)
        '(a (quote b)))

(equals (ssyntax-build-sexpr '((#\b) #\. (#\a)) 'a.b)
        '(a b))

(equals (ssyntax-build-sexpr '((#\c) #\. (#\b) #\. (#\a)) 'a.b.c)
        '((a b) c))

(equals (ssyntax-expand-sexpr 'a.b)   '(a b))
(equals (ssyntax-expand-sexpr 'a!b)   '(a (quote b)))
(equals (ssyntax-expand-sexpr 'a.b.c) '((a b) c))
(equals (ssyntax-expand-sexpr 'a!b.c) '((a (quote b)) c))

(equals (ssyntax-expand-and 'a&b)   '(andf a b))
(equals (ssyntax-expand-and 'a&b&c) '(andf a b c))

(equals (expand-ssyntax 'x!1) '(x (quote 1)))
