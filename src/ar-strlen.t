(use topvar true is)

(true (is (ar-strlen "abc") 3))

(true (is (ar-strlen (ar-symstr (ar-uniq nil nil)))
          16))
