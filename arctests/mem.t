(use arcbase mem equals square-fn)

(equals (mem 'x '(a b)) nil)
(equals (mem 'a '(a b)) '(a b))
(equals (mem 'b '(a b)) '(b))

(equals (mem no '(a nil)) '(nil))

(equals (mem [isa _ 'int] '(a b 2 c d)) '(2 c d))
