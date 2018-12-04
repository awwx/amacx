(use arcboot curly-table equals iso-table obj)

(equals (let a 8
          {a, b 9})
        (obj a 8 b 9))
