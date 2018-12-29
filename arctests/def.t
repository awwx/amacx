(use arcbase def tostring equals)

(equals (errtostring
          (def QeSGdwp0kmLy () 123)
          (def QeSGdwp0kmLy () 456))
        "*** redefining QeSGdwp0kmLy\n")

(equals (QeSGdwp0kmLy) 456)

(assign QeSGdwp0kmLy nil)
