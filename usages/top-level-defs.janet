(import ../src/jipper :as j)

(comment

  (def an-import
    "(import ./analyze :as a)")

  (def a-def
    "(def a 1)")

  (def a-var
    "(var b {:a 1 :b 2})")

  (def a-comment
    (string "(comment\n"
            "\n"
            "  (def c 3)\n"
            "\n"
            "  c\n"
            "  # =>\n"
            "  3\n"
            "\n"
            "  )"))

  (def a-defn-
    (string "(defn- f\n"
            "  [x]\n"
            "  (def b 2)\n"
            "  (defn c\n"
            "    [y]\n"
            "    (+ y b))\n"
            "  (c x))"))

  (def a-defmacro-
    (string "(defmacro- median-of-three\n"
            "  [x y z]\n"
            "  ~(if (<= ,x ,y)\n"
            "     (if (<= ,y ,z) ,y (if (<= ,z ,x) ,x ,z))\n"
            "     (if (<= ,z ,y) ,y (if (<= ,x ,z) ,x ,z))))"))

  (def a-main-defn
    (string "(defn main\n"
            "  [& args]\n"
            `  (f 9))`))

  (def src
    (string an-import "\n"
            "\n"
            a-def "\n"
            "\n"
            a-defn- "\n"
            "\n"
            a-comment "\n"
            "\n"
            a-defmacro- "\n"
            "\n"
            a-var "\n"
            "\n"
            a-main-defn))

  (def results @[])

  (var cur-zloc
    (j/zip-down (j/par src)))

  # XXX: defglobal and varglobal...
  (def def-things
    {"def" 1 "def-" 1
     "var" 1 "var-" 1 
     "defn" 1 "defn-" 1
     "defmacro" 1 "defmacro-" 1
     "varfn" 1})

  # can find top-level "defs" by successively moving "right"
  #
  # XXX: things that get `upscoped` will not be found this way though
  (while cur-zloc
    (when (match (j/node cur-zloc) [:tuple]
            (when-let [child-zloc (j/down cur-zloc)]
              # XXX: assumes first child is a symbol
              (match (j/node child-zloc) [:symbol _ name]
                (when (get def-things name)
                  (array/push results cur-zloc))))))
    (set cur-zloc (j/right cur-zloc)))
  
  (map |(j/gen (j/node $)) results)
  # =>
  @[a-def
    a-defn-
    a-defmacro-
    a-var
    a-main-defn]

  )

