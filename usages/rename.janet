(import ../src/jipper :as j)

# XXX: defglobal and varglobal...
(def def-things
  {"def" 1 "def-" 1
   "var" 1 "var-" 1
   "defn" 1 "defn-" 1
   "defmacro" 1 "defmacro-" 1
   "varfn" 1})

(comment

  # target symbol for renaming
  (def target-sym "f")

  # new name, prefixed with `a^`
  (def new-sym "a^f")

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
    (string "(defn- " target-sym "\n"
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
            "  (" target-sym " 9))"))

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

  (var cur-zloc
    (j/zip-down (j/par src)))

  # XXX: should only look within tuples for symbol to rename?
  #      strictly speaking no.  but not within parameter tuples?  the
  #      parameter tuple exception might be avoided by ensuring that
  #      parameter names never collide with non-parameter names?
  (while (not (j/end? cur-zloc))
    (when (match (j/node cur-zloc) [:symbol _ name]
            (= target-sym name))
      (set cur-zloc (j/replace cur-zloc [:symbol @{} new-sym])))
    (set cur-zloc (j/df-next cur-zloc)))

  (def a-new-defn-
    (string "(defn- " new-sym "\n"
            "  [x]\n"
            "  (def b 2)\n"
            "  (defn c\n"
            "    [y]\n"
            "    (+ y b))\n"
            "  (c x))"))

  (def a-new-main-defn
    (string "(defn main\n"
            "  [& args]\n"
            "  (" new-sym " 9))"))

  (j/gen (j/root cur-zloc))
  # =>
  (string an-import "\n"
          "\n"
          a-comment-opener "\n"
          "\n"
          a-def "\n"
          "\n"
          a-comment-closer "\n"
          "\n"
          a-new-defn- "\n"
          "\n"
          a-comment-opener "\n"
          "\n"
          a-defmacro- "\n"
          "\n"
          a-var "\n"
          "\n"
          a-comment-closer "\n"
          "\n"
          a-new-main-defn)

  )

(defn valid-sym-name?
  [sym-name]
  (peg/match
    ~(sequence (some (choice (range "09" "AZ" "az" "\x80\xFF")
                             (set "!$%&*+-./:<?=>@^_")))
               -1)
    sym-name))

(comment

  (valid-sym-name? "hello")
  # =>
  @[]

  (valid-sym-name? "(hi)")
  # =>
  nil

  )

(def non-call-things
  {"def" 1 "def-" 1
   "var" 1 "var-" 1})

(def call-things
  {"defn" 1 "defn-" 1
   "defmacro" 1 "defmacro-" 1
   "varfn" 1})

# XXX: what about defglobal and varglobal?
(def def-things
  (merge non-call-things call-things))

(def destruct-types
  (invert [:tuple :bracket-tuple
           :array :bracket-array
           :struct :table]))

(defn analyze-defish
  [acc a-zloc]
  (when-let [b-zloc (j/right-skip-wsc a-zloc)]
    (def [_ _ def-type] (j/node a-zloc))
    (def [_ loc _] (j/node (j/up a-zloc)))
    (match (j/node b-zloc)
      [:symbol _ name]
      (array/push acc {:name name
                       :type def-type
                       :loc loc})
      #
      [node-type _ & rest]
      (when (get destruct-types node-type)
        (array/concat acc
                      (keep (fn [[node-type _ node-value]]
                              (when (= :symbol node-type)
                                {:name node-value
                                 :type def-type
                                 :loc loc}))
                            rest)))))
  #
  acc)

(defn find-top-level-syms
  [zloc]
  (var cur-zloc zloc)
  (def sym-zlocs @[])
  #
  (while cur-zloc
    (when (match (j/node cur-zloc) [:tuple]
            (when-let [child-zloc (j/down cur-zloc)]
              # XXX: assumes first child is a symbol
              (match (j/node child-zloc) [:symbol _ name]
                (when (get def-things name)
                  (array/push sym-zlocs child-zloc))))))
    (set cur-zloc (j/right cur-zloc)))
  #
  sym-zlocs)

(defn main
  [_ & args]
  (def prefix (get args 0))
  (assert (and prefix (not (empty? prefix)))
          "please specify a prefix")
  #
  (def prefix-str (string prefix "/"))
  #
  (def src-path (get args 1))
  (assert (and src-path (= :file (os/stat src-path :mode)))
          "please specify a file path for a source file")
  #
  (def src (slurp src-path))
  (def tree (j/par src))
  (var cur-zloc nil)
  #
  (set cur-zloc
    (try
      (j/zip-down tree)
      ([e]
        (eprintf e)
        (eprintf "failed to create zipper from file: %s" src-path)
        (os/exit 1))))
  # find top-level symbols
  (def sym-zlocs (find-top-level-syms cur-zloc))
  (def sym-bits (reduce analyze-defish @[] sym-zlocs))
  (def sym-tbl (invert (map |(get $ :name) sym-bits)))
  # if it worked before, it should work again without error
  (set cur-zloc (j/zip-down tree))
  # rename using found top-level symbols
  (while (not (j/end? cur-zloc))
    (when-let [found (match (j/node cur-zloc) [:symbol _ name]
                       (when (get sym-tbl name)
                         name))]
      (def new-name (string prefix-str found))
      (set cur-zloc (j/replace cur-zloc [:symbol @{} new-name])))
    (set cur-zloc (j/df-next cur-zloc)))
  #
  (print (j/gen (j/root cur-zloc))))

