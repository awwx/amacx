(def type (x)
  (if (a-tagged x)     (ar-tag-type x)
      (acons x)        'cons
      (a-sym x)        'sym
      (a-fn x)         'fn
      (a-char x)       'char
      (a-str x)        'string
      (an-int x)       'int
      (a-num x)        'num
      (a-table x)      'table
      (an-output x)    'output
      (an-input x)     'input
      (a-socket x)     'socket
      (an-exception x) 'exception
      (a-thread x)     'thread
      'unknown))
