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
    (j/search-from zloc 
                   |(match (j/node $) [:symbol _ name]
                      (when (= "a" name)
                        $))))

  (j/node cur-zloc)
  # =>
  [:symbol @{:bc 23 :bl 1 :ec 24 :el 1} "a"]

  # first edit
  (def edit-1-zloc
    (j/replace cur-zloc [:symbol @{} "b"]))

  (j/node edit-1-zloc)
  # =>
  [:symbol @{} "b"]

  (def cur-2-zloc
    (j/search-from edit-1-zloc
                   |(match (j/node $) [:string _ value]
                      (when (= `"hi"` value)
                        $))))

  (j/node cur-2-zloc)
  # =>
  [:string @{:bc 10 :bl 5 :ec 14 :el 5} `"hi"`]

  # second edit
  (def edit-2-zloc
    (j/replace cur-2-zloc [:string @{} `"smile!"`]))

  (j/node edit-2-zloc)
  # =>
  [:string @{} `"smile!"`]

  (j/gen (j/root edit-2-zloc))
  # =>
  (string "(import ./analyze :as b)\n"
          "\n"
          "(defn main\n"
          "  [& args]\n"
          `  (print "smile!"))`)

  )

