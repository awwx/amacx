(use equals tostring pr)

(equals (tostring (pr "hi")
                  (pr "123"))
        "hi123")
