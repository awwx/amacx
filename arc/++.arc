(use arcbase complex-fn setforms w/uniq +list)

(mac ++ (place (o i 1))
  (if (isa place 'sym)
      `(,= ,place (,+ ,place ,i))
      (w/uniq gi
        (let (binds val setter) (setforms place)
          `(,withs ,(+ binds (list gi i))
             (,setter (,+ ,val ,gi)))))))

(mac -- (place (o i 1))
  (if (isa place 'sym)
      `(,= ,place (,- ,place ,i))
      (w/uniq gi
        (let (binds val setter) (setforms place)
          `(,withs ,(+ binds (list gi i))
             (,setter (,- ,val ,gi)))))))
