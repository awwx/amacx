(use equals findfile)

(equals (findfile rootdir '("arc" "src") "list.arc")
        "src/list.arc")

(equals (findsrc nil 'w/uniq)
        (+ rootdir "arc/w_slash_uniq.arc"))

(equals (findtest nil 'literal)
        (+ rootdir "src/literal.t"))
