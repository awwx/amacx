(use arcbase tostring simple-equals writec)

(equals (tostring (writec #\A)
                  (writec #\B))
        "AB")
