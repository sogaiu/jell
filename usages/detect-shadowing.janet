(import ../src/jipper :as j)

(comment

  (def destruct-types
    (invert [:tuple :bracket-tuple
             :array :bracket-array
             :struct :table]))

  (defn analyze-defish
    [a-zloc]
    (def acc @[])
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

  (analyze-defish (-> (j/par "(def a 1)")
                      j/zip-down
                      j/down))
  # =>
  @[{:loc @{:bc 1 :bl 1 :ec 10 :el 1} 
     :name "a" 
     :type "def"}]

  (analyze-defish (-> (j/par "(def [a b] [1 2])")
                      j/zip-down
                      j/down))
  # =>
  @[{:loc @{:bc 1 :bl 1 :ec 18 :el 1} 
     :name "a" 
     :type "def"} 
    {:loc @{:bc 1 :bl 1 :ec 18 :el 1} 
     :name "b" 
     :type "def"}]

  (analyze-defish (-> (j/par "(def {:x x :y y} {:x 1 :y 0})")
                      j/zip-down
                      j/down))
  # =>
  @[{:loc @{:bc 1 :bl 1 :ec 30 :el 1} 
     :name "x" 
     :type "def"} 
    {:loc @{:bc 1 :bl 1 :ec 30 :el 1} 
     :name "y" 
     :type "def"}]

  )

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
            a-var "\n"
            "\n"
            a-def-with-tuple-destructuring "\n"
            "\n"
            a-var-with-struct-destructuring "\n"
            "\n"
            a-comment "\n"
            "\n"
            a-defmacro- "\n"
            "\n"
            a-defn- "\n"
            "\n"
            a-main-defn))

  # use ajrepl-send-expression-upscoped for this
  (var cur-zloc
    (j/zip-down (j/par src)))

  (def non-call-things
    {"def" 1 "def-" 1
     "var" 1 "var-" 1})

  (def call-things
    {"defn" 1 "defn-" 1
     "defmacro" 1 "defmacro-" 1
     "varfn" 1})

  # XXX: defglobal and varglobal...
  (def def-things
    (merge non-call-things
           call-things))

  (def calls @[])

  (def non-calls @[])

  (def things @{})

  (def shadowing @[])

  # XXX: what about names from elsewhere...
  #
  #      * parameter tuples
  #      * built-in macros that introduce symbol names
  #      * user-defined macros
  (while (not (j/end? cur-zloc))
    (when (match (j/node cur-zloc) [:tuple]
            (when-let [child-zloc (j/down cur-zloc)]
              # XXX: assumes first child is a symbol (e.g. `def`)
              (match (j/node child-zloc) [:symbol _ name]
                (cond
                  (get call-things name)
                  (let [res (analyze-defish child-zloc)]
                    (each r res
                      (def a-name (get r :name))
                      (when-let [already (get things a-name)]
                        (array/push shadowing [r already]))
                      (put things a-name r))
                    (array/concat calls res))
                  #
                  (get non-call-things name)
                  (let [res (analyze-defish child-zloc)]
                    (each r res
                      (def a-name (get r :name))
                      (when-let [already (get things a-name)]
                        (array/push shadowing [r already]))
                      (put things a-name r))
                    (array/push non-calls child-zloc)))))))
    (set cur-zloc (j/df-next cur-zloc)))

  things
  # =>
  @{"a" {:loc @{:bc 1 :bl 1 :ec 10 :el 1} :name "a" :type "def"}
    "b" {:loc @{:bc 3 :bl 27 :ec 12 :el 27} :name "b" :type "def"}
    "c" {:loc @{:bc 3 :bl 28 :ec 13 :el 30} :name "c" :type "defn"}
    "f" {:loc @{:bc 1 :bl 25 :ec 9 :el 31} :name "f" :type "defn-"}
    "i" {:loc @{:bc 1 :bl 7 :ec 30 :el 7} :name "i" :type "var"}
    "j" {:loc @{:bc 1 :bl 7 :ec 30 :el 7} :name "j" :type "var"}
    "main" {:loc @{:bc 1 :bl 33 :ec 9 :el 35}
            :name "main"
            :type "defn"}
    "median-of-three" {:loc @{:bc 1 :bl 19 :ec 48 :el 23} 
                       :name "median-of-three" 
                       :type "defmacro-"}
    "x" {:loc @{:bc 1 :bl 5 :ec 18 :el 5} :name "x" :type "def"}
    "y" {:loc @{:bc 1 :bl 5 :ec 18 :el 5} :name "y" :type "def"}}

  shadowing
  # =>
  @[[{:loc @{:bc 3 :bl 27 :ec 12 :el 27} :name "b" :type "def"} 
     {:loc @{:bc 1 :bl 3 :ec 20 :el 3} :name "b" :type "var"}] 
    [{:loc @{:bc 3 :bl 28 :ec 13 :el 30} :name "c" :type "defn"} 
     {:loc @{:bc 3 :bl 11 :ec 12 :el 11} :name "c" :type "def"}]]

  # show "def type"s and names of non-call things
  (reduce (fn [acc a-zloc]
            (def res (analyze-defish a-zloc))
            (array/concat acc res))
          @[]
          non-calls)
  # =>
  @[{:loc @{:bc 1 :bl 1 :ec 10 :el 1} :name "a" :type "def"}
    {:loc @{:bc 1 :bl 3 :ec 20 :el 3} :name "b" :type "var"} 
    {:loc @{:bc 1 :bl 5 :ec 18 :el 5} :name "x" :type "def"} 
    {:loc @{:bc 1 :bl 5 :ec 18 :el 5} :name "y" :type "def"}
    {:loc @{:bc 1 :bl 7 :ec 30 :el 7} :name "i" :type "var"}
    {:loc @{:bc 1 :bl 7 :ec 30 :el 7} :name "j" :type "var"}
    {:loc @{:bc 3 :bl 11 :ec 12 :el 11} :name "c" :type "def"}
    {:loc @{:bc 3 :bl 27 :ec 12 :el 27} :name "b" :type "def"}]

  )

