(import ./jipper :prefix "")
(import ./utils :prefix "")

(defn c/is-import?
  [zloc]
  (def node (j/node zloc))
  (when (not= :tuple (get node 0))
    (break false))
  #
  (def head-zloc (j/down zloc))
  (when (not head-zloc)
    (break false))
  #
  (def first-child-node (j/node head-zloc))
  (when (not first-child-node)
    (break false))
  #
  (def fc-node-type (get first-child-node 0))
  (def first-non-wsc-node
    (cond
      (= :symbol fc-node-type)
      first-child-node
      #
      (or (= :whitespace fc-node-type)
          (= :comment fc-node-type))
      (-> (j/right-skip-wsc head-zloc)
          j/node)
      #
      nil))
  (when (not first-non-wsc-node)
    (break false))
  #
  (when (= "import" (get first-non-wsc-node 2))
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

  (c/is-import? zloc)
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

  (c/is-import? zloc)
  # =>
  true

  )

(defn c/analyze-import
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

  (c/analyze-import [:tuple @{:bc 1 :bl 1 :ec 27 :el 1}
                   [:symbol @{:bc 2 :bl 1 :ec 8 :el 1} "import"]
                   [:whitespace @{:bc 8 :bl 1 :ec 9 :el 1} " "]
                   [:symbol @{:bc 9 :bl 1 :ec 15 :el 1} "./args"]
                   [:whitespace @{:bc 15 :bl 1 :ec 16 :el 1} " "]
                   [:keyword @{:bc 16 :bl 1 :ec 23 :el 1} ":prefix"]
                   [:whitespace @{:bc 23 :bl 1 :ec 24 :el 1} " "]
                   [:string @{:bc 24 :bl 1 :ec 26 :el 1} "\"\""]])
  # =>
  @{:path "./args" :prefix ""}

  (c/analyze-import [:tuple @{:bc 1 :bl 1 :ec 23 :el 1}
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

(defn c/make-version-def-form
  [&opt name time]
  (default name "version")
  #
  (string/format `(def %s "%s")` name (u/dt-stamp time)))

(comment

  (c/make-version-def-form nil 0)
  # =>
  "(def version \"1970-01-01_00-00-00\")"

  (c/make-version-def-form "my-stamp" "1234567890")
  # =>
  "(def my-stamp \"2009-02-13_23-31-30\")"

  )

(defn c/is-version-def?
  [zloc]
  (def node (j/node zloc))
  (when (not= :tuple (get node 0))
    (break false))
  #
  (def head-zloc (j/down zloc))
  (when (not head-zloc)
    (break false))
  #
  (def first-child-node (j/node head-zloc))
  (def fc-node-type (get first-child-node 0))
  (def first-non-wsc-zloc
    (cond
      (= :symbol fc-node-type)
      head-zloc
      #
      (or (= :whitespace fc-node-type)
          (= :comment fc-node-type))
      (j/right-skip-wsc head-zloc)
      #
      nil))
  (when (not first-non-wsc-zloc)
    (break false))
  #
  (def [_ _ fnw-type] (j/node first-non-wsc-zloc))
  (when (not= "def" fnw-type)
    (break false))
  #
  (def next-non-wsc-zloc (j/right-skip-wsc first-non-wsc-zloc))
  (when (not next-non-wsc-zloc)
    (break false))
  #
  (def [nnw-type _ nnw-value] (j/node next-non-wsc-zloc))
  (when (and (= :symbol nnw-type)
             (= "version" nnw-value))
    (u/maybe-dump :is-version-def? (j/gen (j/node zloc)))
    true))

(comment

  (def zloc (j/zip-down (j/par `(def version "DEVEL")`)))

  (j/node zloc)
  # =>
  [:tuple @{:bc 1 :bl 1 :ec 22 :el 1}
   [:symbol @{:bc 2 :bl 1 :ec 5 :el 1} "def"]
   [:whitespace @{:bc 5 :bl 1 :ec 6 :el 1} " "]
   [:symbol @{:bc 6 :bl 1 :ec 13 :el 1} "version"]
   [:whitespace @{:bc 13 :bl 1 :ec 14 :el 1} " "]
   [:string @{:bc 14 :bl 1 :ec 21 :el 1} "\"DEVEL\""]]

  (c/is-version-def? zloc)
  # =>
  true

  )

