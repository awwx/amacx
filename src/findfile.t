(use equals findfile rootdir)

(equals (findfile rootdir '("arcsrc" "src") "list.arc")
        "src/list.arc")
