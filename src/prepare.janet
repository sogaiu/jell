(import ./common :as c)
(import ./jipper :as j)

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

# XXX: does not handle arbitrarily nested destructuring
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

(defn tweak-import-forms
  [zloc]
  (var cur-zloc zloc)
  (set cur-zloc (j/zip-down (j/root cur-zloc)))
  (while (not (j/end? cur-zloc))
    (when-let [i-zloc (match (j/node cur-zloc)
                        [:tuple _ [:symbol _ "import"]]
                        cur-zloc)
               i-node (j/node i-zloc)]
      (def i-tbl (c/analyze-import i-node))
      (assertf (get i-tbl :path)
               "import form lacks a path: %n" i-tbl)
      (set cur-zloc (j/replace cur-zloc
                               [:tuple @{}
                                [:symbol @{} "import"]
                                [:whitespace @{} " "]
                                [:symbol @{} (get i-tbl :path)]
                                [:whitespace @{} " "]
                                [:keyword @{} ":prefix"]
                                [:whitespace @{} " "]
                                [:string @{} `""`]])))
    (set cur-zloc (j/df-next cur-zloc)))
  #
  cur-zloc)

(defn prepare
  [prefix in-path out-path]
  (def prefix-str (string prefix "/"))
  #
  (def src (slurp in-path))
  (def tree (j/par src))
  (var cur-zloc nil)
  #
  (set cur-zloc
       (try (j/zip-down tree)
         ([e] (eprintf e)
              (eprintf "failed to create zipper from file: %s" in-path)
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
  # replace import forms
  (set cur-zloc (tweak-import-forms cur-zloc))
  #
  (spit out-path (j/gen (j/root cur-zloc))))

(defn prepare-imported
  [in-dir obj-path prefixes opts]
  (def {:sep sep} opts)
  (eachp [path prefix] prefixes
    (def fname (string path ".janet"))
    (def ipath (string in-dir sep fname))
    (def opath (string obj-path sep fname))
    (prepare prefix ipath opath)))

(defn prepare-start
  [start-path in-name obj-path opts]
  (def {:sep sep :start-file-perm perm} opts)
  (def in-src (slurp start-path))
  (def in-tree (j/par in-src))
  (var cur-zloc nil)
  (set cur-zloc
       (try (j/zip-down in-tree)
         ([e] (eprintf e)
              (eprintf "zipper creation failed for file: %s" start-path)
              (os/exit 1))))
  (def in-out-path (string obj-path sep in-name))
  (spit in-out-path
        (j/gen (j/root (tweak-import-forms cur-zloc))))
  (when perm
    (os/chmod in-out-path perm))
  #
  in-out-path)

