(import ../src/jipper :as j)

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

(comment

  (def a-def
    "(def a 1)")

  (def a-def-with-tuple-destructuring
    "(def [x y] [8 9])")

  (def a-var-with-struct-destructuring
    "(var {:i i :j j} {:i 1 :j 0})")

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
    (string a-def "\n"
            "\n"
            a-defn- "\n"
            "\n"
            a-def-with-tuple-destructuring "\n"
            "\n"
            a-var-with-struct-destructuring "\n"
            "\n"
            a-comment "\n"
            "\n"
            a-defmacro- "\n"
            "\n"
            a-var "\n"
            "\n"
            a-main-defn))

  (var cur-zloc
    (j/zip-down (j/par src)))

  (def non-calls @[])

  (def calls @[])

  # find effectively top-level defish things and their names
  (while cur-zloc
    (when (match (j/node cur-zloc) [:tuple]
            (when-let [child-zloc (j/down cur-zloc)]
              # XXX: assumes first child is a symbol
              (match (j/node child-zloc) [:symbol _ name]
                (cond
                  (get call-things name)
                  (array/push calls child-zloc)
                  #
                  (get non-call-things name)
                  (array/push non-calls child-zloc))))))
    (set cur-zloc (j/right cur-zloc)))

  # nodes of the symbol portion of the detected non-calls
  (map j/node non-calls)
  # =>
  @[[:symbol @{:bc 2 :bl 1 :ec 5 :el 1} "def"]
    [:symbol @{:bc 2 :bl 11 :ec 5 :el 11} "def"]
    [:symbol @{:bc 2 :bl 13 :ec 5 :el 13} "var"]
    [:symbol @{:bc 2 :bl 31 :ec 5 :el 31} "var"]]

  # "def type"s and names of non-call things
  (reduce analyze-defish
          @[]
          non-calls)
  # =>
  @[{:loc @{:bc 1 :bl 1 :ec 10 :el 1} :name "a" :type "def"}
    {:loc @{:bc 1 :bl 11 :ec 18 :el 11} :name "x" :type "def"}
    {:loc @{:bc 1 :bl 11 :ec 18 :el 11} :name "y" :type "def"}
    {:loc @{:bc 1 :bl 13 :ec 30 :el 13} :name "i" :type "var"}
    {:loc @{:bc 1 :bl 13 :ec 30 :el 13} :name "j" :type "var"}
    {:loc @{:bc 1 :bl 31 :ec 20 :el 31} :name "b" :type "var"}]

  (map j/node calls)
  # =>
  @[[:symbol @{:bc 2 :bl 3 :ec 7 :el 3} "defn-"]
    [:symbol @{:bc 2 :bl 25 :ec 11 :el 25} "defmacro-"]
    [:symbol @{:bc 2 :bl 33 :ec 6 :el 33} "defn"]]

  # "def type"s and names of call things
  (reduce analyze-defish
          @[]
          calls)
  # =>
  @[{:loc @{:bc 1 :bl 3 :ec 9 :el 9}
     :name "f"
     :type "defn-"}
    {:loc @{:bc 1 :bl 25 :ec 48 :el 29}
     :name "median-of-three"
     :type "defmacro-"}
    {:loc @{:bc 1 :bl 33 :ec 9 :el 35}
     :name "main"
     :type "defn"}]

  )

(defn main
  [_ & args]
  (def path (get args 0))
  (assert path "expected a path as an argument")
  (assertf (= :file (os/stat path :mode))
           "expected file for: %s" path)
  #
  (var cur-zloc nil)
  (def src (slurp path))
  (def tree (j/par src))
  # find top-level defish things
  (set cur-zloc (j/zip-down tree))
  (def tl-things @[])
  (while cur-zloc
    (when (match (j/node cur-zloc) [:tuple]
          (when-let [child-zloc (j/down cur-zloc)]
            # XXX: assumes first child is a symbol
            (match (j/node child-zloc) [:symbol _ name]
              (when (get def-things name)
                (array/push tl-things child-zloc))))))
    (set cur-zloc (j/right cur-zloc)))
  #
  (def results
    (reduce analyze-defish @[] tl-things))
  #
  (print "# top-level definitions")
  (each r results
    (pp r))
  # find comment "top-level" defish things
  (set cur-zloc (j/zip-down tree))
  (def ctl-things @[])
  (while cur-zloc
    (when (match (j/node cur-zloc) [:tuple _ [:symbol _ "comment"]]
            (let [child-zloc (j/down cur-zloc)]
              (var in-zloc (j/right child-zloc))
              (while in-zloc
                (match (j/node in-zloc) [:tuple _ [:symbol _ name]]
                  (when (get def-things name)
                    (array/push ctl-things
                                (j/down in-zloc))))
                (set in-zloc (j/right in-zloc))))))
    (set cur-zloc (j/right cur-zloc)))
  #
  (def c-results
    (reduce analyze-defish @[] ctl-things))
  #
  (print "# comment top-level definitions")
  (each cr c-results
    (pp cr))

  )

