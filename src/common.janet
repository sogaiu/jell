(import ./jipper :as j)
(import ./utils :as u)

(defn is-import?
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

  (is-import? zloc)
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

  (is-import? zloc)
  # =>
  true

  )

(defn analyze-import
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

  (analyze-import [:tuple @{:bc 1 :bl 1 :ec 27 :el 1}
                   [:symbol @{:bc 2 :bl 1 :ec 8 :el 1} "import"]
                   [:whitespace @{:bc 8 :bl 1 :ec 9 :el 1} " "]
                   [:symbol @{:bc 9 :bl 1 :ec 15 :el 1} "./args"]
                   [:whitespace @{:bc 15 :bl 1 :ec 16 :el 1} " "]
                   [:keyword @{:bc 16 :bl 1 :ec 23 :el 1} ":prefix"]
                   [:whitespace @{:bc 23 :bl 1 :ec 24 :el 1} " "]
                   [:string @{:bc 24 :bl 1 :ec 26 :el 1} "\"\""]])
  # =>
  @{:path "./args" :prefix ""}

  (analyze-import [:tuple @{:bc 1 :bl 1 :ec 23 :el 1}
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

