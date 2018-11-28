(use mac complex-fn if isa setforms + w/uniq let withs +list)

(mac ++ (place (o i 1))
  (if (isa place 'sym)
      `(,= ,place (,+ ,place ,i))
      (w/uniq gi
        (let (binds val setter) (setforms place)
          `(,withs ,(+ binds (list gi i))
             (,setter (,+ ,val ,gi)))))))
