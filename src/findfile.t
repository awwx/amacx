(use equals findfile rootdir)

(equals (findfile rootdir '("arcsrc" "src") "list.arc")
        "src/list.arc")

(equals (findsrc nil 'w/uniq)
        "arcsrc/w_slash_uniq.arc")

(equals (findtest nil 'literal)
        "src/literal.t")
