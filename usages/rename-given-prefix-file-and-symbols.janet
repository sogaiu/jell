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
  (def targets (slice args 2))
  #
  (each t targets
    (assertf (valid-sym-name? t) "not a valid symbol name: %s" t))
  #
  (def sym-tbl (tabseq [s :in targets] s 1))
  #
  (var cur-zloc
    (try
      (j/zip-down (j/par (slurp src-path)))
      ([e]
        (eprintf e)
        (eprintf "failed to create zipper from file: %s" src-path)
        (os/exit 1))))
  #
  (while (not (j/end? cur-zloc))
    (when-let [found (match (j/node cur-zloc) [:symbol _ name]
                       (when (get sym-tbl name)
                         name))]
      (def new-name (string prefix-str found))
      (set cur-zloc (j/replace cur-zloc [:symbol @{} new-name])))
    (set cur-zloc (j/df-next cur-zloc)))
  #
  (print (j/gen (j/root cur-zloc))))

