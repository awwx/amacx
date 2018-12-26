(use equals findfile rootdir)

(equals (findfile rootdir '("arc" "src") "list.arc")
        "src/list.arc")

(equals (findsrc nil 'w/uniq)
        "arc/w_slash_uniq.arc")

(equals (findtest nil 'literal)
        "src/literal.t")
