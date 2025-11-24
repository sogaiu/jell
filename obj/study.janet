(import ./jipper :prefix "")
(import ./utils :prefix "")

(defn s/is-import?
  [zloc]
  (def node (j/node zloc))
  (when (not= :tuple (get node 0))
    (break false))
  #
  (def head-zloc (j/down zloc))
  (when (not head-zloc)
    (break false))
  #
  (def first-sym-node
    (if-let [a-node (j/node head-zloc)
             _ (= :symbol (get a-node 0))]
      a-node
      (when-let [res
                 (j/right-until head-zloc |(match (j/node $)
                                             [:symbol]
                                             $))]
        (j/node res))))
  (when (not first-sym-node)
    (break false))
  #
  (when (= "import" (get first-sym-node 2))
    (u/maybe-dump :is-import? (j/gen (j/node zloc)))
    true))

(comment

  (def zloc (j/zip-down (j/par `(import ./args :prefix "")`)))

  (j/node zloc)
  # =>
  [:tuple @{:bc 1 :bl 1 :ec 27 :el 1}
   [:symbol @{:bc 2 :bl 1 :ec 8 :el 1} "import"]
   [:whitespace @{:bc 8 :bl 1 :ec 9 :el 1} " "]
   [:symbol @{:bc 9 :bl 1 :ec 15 :el 1} "./args"]
   [:whitespace @{:bc 15 :bl 1 :ec 16 :el 1} " "]
   [:keyword @{:bc 16 :bl 1 :ec 23 :el 1} ":prefix"]
   [:whitespace @{:bc 23 :bl 1 :ec 24 :el 1} " "]
   [:string @{:bc 24 :bl 1 :ec 26 :el 1} `""`]]

  (s/is-import? zloc)
  # =>
  true

  (def zloc (j/zip-down (j/par `( import ./args :as a)`)))

  (j/node zloc)
  # =>
  [:tuple @{:bc 1 :bl 1 :ec 23 :el 1}
   [:whitespace @{:bc 2 :bl 1 :ec 3 :el 1} " "]
   [:symbol @{:bc 3 :bl 1 :ec 9 :el 1} "import"]
   [:whitespace @{:bc 9 :bl 1 :ec 10 :el 1} " "]
   [:symbol @{:bc 10 :bl 1 :ec 16 :el 1} "./args"]
   [:whitespace @{:bc 16 :bl 1 :ec 17 :el 1} " "]
   [:keyword @{:bc 17 :bl 1 :ec 20 :el 1} ":as"]
   [:whitespace @{:bc 20 :bl 1 :ec 21 :el 1} " "]
   [:symbol @{:bc 21 :bl 1 :ec 22 :el 1} "a"]]

  (s/is-import? zloc)
  # =>
  true

  )

(defn s/analyze-import
  [node]
  (def parsed
    (try (parse (j/gen node))
      ([e] (errorf "failed to parse node: %n" node))))
  (assertf (tuple? parsed) "expected tuple, found: %n: for: %n"
           (type parsed) parsed)
  #
  (table :path (string (get parsed 1))
         ;(tuple/slice parsed 2)))

(comment

  (s/analyze-import [:tuple @{:bc 1 :bl 1 :ec 27 :el 1}
                   [:symbol @{:bc 2 :bl 1 :ec 8 :el 1} "import"]
                   [:whitespace @{:bc 8 :bl 1 :ec 9 :el 1} " "]
                   [:symbol @{:bc 9 :bl 1 :ec 15 :el 1} "./args"]
                   [:whitespace @{:bc 15 :bl 1 :ec 16 :el 1} " "]
                   [:keyword @{:bc 16 :bl 1 :ec 23 :el 1} ":prefix"]
                   [:whitespace @{:bc 23 :bl 1 :ec 24 :el 1} " "]
                   [:string @{:bc 24 :bl 1 :ec 26 :el 1} "\"\""]])
  # =>
  @{:path "./args" :prefix ""}

  (s/analyze-import [:tuple @{:bc 1 :bl 1 :ec 23 :el 1}
                   [:whitespace @{:bc 2 :bl 1 :ec 3 :el 1} " "]
                   [:symbol @{:bc 3 :bl 1 :ec 9 :el 1} "import"]
                   [:whitespace @{:bc 9 :bl 1 :ec 10 :el 1} " "]
                   [:symbol @{:bc 10 :bl 1 :ec 16 :el 1} "./args"]
                   [:whitespace @{:bc 16 :bl 1 :ec 17 :el 1} " "]
                   [:keyword @{:bc 17 :bl 1 :ec 20 :el 1} ":as"]
                   [:whitespace @{:bc 20 :bl 1 :ec 21 :el 1} " "]
                   [:symbol @{:bc 21 :bl 1 :ec 22 :el 1} "a"]])
  # =>
  @{:as 'a :path "./args"}

  )

(defn s/find-files-and-imports
  [in-path]
  # assumes paths are full paths...
  # XXX: could check if we had abspath?
  (def [dir-path file-path] (u/split-path in-path))
  # remember which files have already been "imported"
  (def seen @{})
  (def imports @{})
  # for restoring the current working directory (cwd)
  (def old-dir (os/cwd))
  # need to operate relative to in-path's dir
  (os/cd dir-path)
  #
  (defer (os/cd old-dir)
    (defn helper
      [a-path]
      (when (in seen a-path) (break))
      #
      (assertf (= :file (os/stat a-path :mode))
               "file does not exist or not a file: %s" a-path)
      (put seen a-path true)
      (def src (slurp a-path))
      (when (not (empty? src))
        (def tree (j/par src))
        (assertf tree "failed to parse: %s" a-path)
        (def zloc (j/zip-down tree))
        (assertf zloc "zip-down failed for tree for path: %s" a-path)
        (var cur-zloc zloc)
        (put imports a-path @[])
        (def import-paths (get imports a-path))
        (while (def i-zloc
                 (j/search-from cur-zloc |(match (j/node $) [:tuple]
                                            (when (s/is-import? $)
                                              $))))
          (set cur-zloc (j/df-next i-zloc))
          (def i-node (j/node i-zloc))
          (def i-stats (s/analyze-import i-node))
          (def i-path (get i-stats :path))
          (assertf (string/has-prefix? "./" i-path)
                   "path should start with `./`, but found: %s" i-path)
          (def j-file (os/realpath (string i-path ".janet")))
          (def prefix
            (cond
              (def as (get i-stats :as))
              (string as)
              #
              (def pfx (get i-stats :prefix))
              pfx
              # remove leading ./
              (string/slice i-path 2)))
          (put imports a-path
               (array/push import-paths [i-path j-file prefix]))
          (helper j-file))))
    #
    (helper (os/realpath file-path))
    #
    imports))

(defn s/study
  [start-path]
  (def files-and-imports (s/find-files-and-imports start-path))
  (u/maybe-dump :files-and-imports files-and-imports)
  #
  (def prefixes
    (reduce (fn [acc i-stats]
              (each [pth _ pfx] i-stats
                (if-let [o-pfx (get acc pth)]
                  (assertf (= pfx o-pfx)
                           "prefixes don't match: %s != %s" pfx o-pfx)
                  (put acc pth pfx)))
              acc)
            @{}
            (values files-and-imports)))
  #
  prefixes)

