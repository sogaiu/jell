(import ../src/jipper :as j)

(comment

  (def an-import
    "(import ./analyze :as a)")

  (def a-def
    "(def a 1)")

  (def a-var
    "(var b {:a 1 :b 2})")

  (def a-comment-opener
    "(comment\n")

  (def a-comment-closer
    "  )")

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
            a-comment-opener "\n"
            "\n"
            a-def "\n"
            "\n"
            a-comment-closer "\n"
            "\n"
            a-defn- "\n"
            "\n"
            a-comment-opener "\n"
            "\n"
            a-defmacro- "\n"
            "\n"
            a-var "\n"
            "\n"
            a-comment-closer "\n"
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

  # can find comment top-level "defs" by finding comment forms
  # and scanning inside their "top-levels"
  #
  # alternative might be to use path and leftmost?
  #
  # possibly of use...match against [:tuple _ [:symbol _ "comment"]]
  (while cur-zloc
    (when (match (j/node cur-zloc) [:tuple]
            (when-let [child-zloc (j/down cur-zloc)]
              # XXX: assumes first child is a symbol
              (match (j/node child-zloc) [:symbol _ name]
                (when (= "comment" name)
                  (var in-zloc (j/right child-zloc))
                  (while in-zloc
                    (when (match (j/node in-zloc) [:tuple]
                            (when-let [in-child-zloc (j/down in-zloc)]
                              # XXX: assumes first child is a symbol
                              (match (j/node in-child-zloc) [:symbol _ in-name]
                                (when (get def-things in-name)
                                  (array/push results in-zloc))))))
                    (set in-zloc (j/right in-zloc))))))))
    (set cur-zloc (j/right cur-zloc)))
  
  (map |(j/gen (j/node $)) results)
  # =>
  @[a-def
    a-defmacro-
    a-var]

  )

