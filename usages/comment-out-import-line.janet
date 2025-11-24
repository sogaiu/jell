(import ../src/jipper :as j)

(comment

  (def src
    (string "(import ./analyze :as a)\n"
            "\n"
            "(defn main\n"
            "  [& args]\n"
            `  (print "hi"))`))

  (def zloc
    (j/zip-down (j/par src)))

  (def cur-zloc
    (j/search-from
      zloc 
      (fn [a-zloc]
        (match (j/node a-zloc) [:tuple]
          (when-let [child-zloc (j/down a-zloc)]
            (if (match (j/node child-zloc) [:symbol _ value]
                  (= value "import"))
              a-zloc
              (when-let [non-wsc-zloc (j/right-skip-wsc child-zloc)]
                (match (j/node non-wsc-zloc) [:symbol _ value]
                  (when (= value "import")
                    non-wsc-zloc)))))))))

  (def import-node (j/node cur-zloc))

  import-node
  # =>
  [:tuple @{:bc 1 :bl 1 :ec 25 :el 1} 
   [:symbol @{:bc 2 :bl 1 :ec 8 :el 1} "import"] 
   [:whitespace @{:bc 8 :bl 1 :ec 9 :el 1} " "] 
   [:symbol @{:bc 9 :bl 1 :ec 18 :el 1} "./analyze"] 
   [:whitespace @{:bc 18 :bl 1 :ec 19 :el 1} " "] 
   [:keyword @{:bc 19 :bl 1 :ec 22 :el 1} ":as"] 
   [:whitespace @{:bc 22 :bl 1 :ec 23 :el 1} " "] 
   [:symbol @{:bc 23 :bl 1 :ec 24 :el 1} "a"]]

  (def import-as-str (j/gen import-node))

  import-as-str
  # =>
  "(import ./analyze :as a)"

  # comment out
  (def new-zloc
    (j/replace cur-zloc [:comment @{} (string "# " import-as-str)]))

  (j/node new-zloc)
  # =>
  [:comment @{} "# (import ./analyze :as a)"]

  (j/gen (j/root new-zloc))
  # =>
  (string "# (import ./analyze :as a)\n"
          "\n"
          "(defn main\n"
          "  [& args]\n"
          `  (print "hi"))`)

  )

