(use arcbase tostring equals writec)

(equals (tostring (writec #\A)
                  (writec #\B))
        "AB")
